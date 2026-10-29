import { useCallback, useContext, useEffect, useId, useRef } from "react";
import { ExportRequestContext } from "../ExportRequestProvider";
import * as z from "@/util/z";
import { RefreshContext } from "../refresh/RefreshProvider";
import { Box, Typography } from "@mui/material";
import uPlot from "uplot";
import unzip from "@/util/unzip";
import defaultSeriesStyle, { SeriesStyle } from "@/util/seriesStyle";
import { useColorScheme } from "@mui/material/styles";
import zipWith from "@/util/zipWith";
import sum from "@/util/sum";
import { getMode } from "../../util/getMode";
import byteFormatter from "@/util/formatter/byte";
import { CursorSyncContext } from "../cursorSync/CursorSyncProvider";

/**
 * The properies for the plot component.
 */
export interface Props<PlotAttrs extends z.Attrs> {
  /**
   * The name of the telemetry signal that is visualised by this plot.
   */
  readonly name: keyof z.ViewerTelemetry;

  /**
   * A predicate that validates that the received attributes have the expected fields.
   */
  readonly validateAttrs: (attrs: any) => attrs is PlotAttrs;

  /**
   * A function that computes the series label from its attributes.
   */
  readonly getSeriesLabel: (attrs: PlotAttrs) => string;

  /**
   * A function that computes the series options from its label.
   */
  readonly getSeriesOptions: (label: string) => Series.Options;

  /**
   * The unit for the y-values of all series in this plot.
   */
  readonly unit?: Unit;

  /**
   * The maximum number of series to visualise in this plot.
   *
   * If otherSeries is defined, any the values of all remaining series
   * will be summarised under that series.
   */
  readonly seriesLimit?: number;

  /**
   * If totalSeris is defined, the plot will include a series that shows
   * the total y-value of all series.
   */
  readonly totalSeries?: Series.Options;

  /**
   * If seriesLimit and otherSeries are both defined, the plot will include
   * a series that shows the total y-value of all omitted series.
   */
  readonly otherSeries?: Series.Options;

  /**
   * If seriesLimit is defined, the plot will use this predicate to select
   * which series are visualised. By default, the series with the highest
   * maximum values are selected.
   */
  readonly seriesPriority?: Series.Priority;

  /**
   * This value is used to assign styles to the visualised series.
   */
  readonly seriesStyle?: SeriesStyle[];

  /**
   * The options for the plot.
   *
   * These are the uPlot options, omitting the options that are computed
   * internally by this plot. This plot has responsive support for the
   * width and height options.
   */
  readonly options: Options;
}

/**
 * The type of aligned data used by the plot.
 */
export type AlignedData = [
  xValues: Array<number>,
  ...yValues: Array<Series.Data>,
];

/**
 * The type of units supported by this plot.
 *
 * The elements of this enumeration are UCUM units.
 */
export type Unit = "By";

/**
 * The type of plot options.
 */
export type Options = Omit<
  uPlot.Options,
  "scales" | "series" | "axes" | "cursor"
> & {
  axes?: Axis[];
  cursor?: Omit<uPlot.Cursor, "sync">;
};

export type Series = [xSeries: uPlot.Series, ...ySeries: uPlot.Series[]];

export namespace Series {
  export type Options = Omit<uPlot.Series, "stroke" | "dash">;

  export type Data = Array<number | null>;

  export type Indices = Partial<Record<string, number>>;

  export type Priority = ([series, data]: [
    Series.Options,
    Series.Data,
  ]) => number;

  export type Formatter = Pick<uPlot.Series, "value">;
}

export type Axis = Omit<uPlot.Axis, "stroke"> & {
  stroke?: Axis.Stroke;
};

export namespace Axis {
  export type Stroke =
    | uPlot.Axis.Stroke
    | { light: uPlot.Axis.Stroke; dark: uPlot.Axis.Stroke };

  export type Formatter = Pick<uPlot.Axis, "size" | "values">;
}

/**
 * Get the formatter for a specific unit.
 */
const getFormatter = (_unit: Unit) => byteFormatter;

/**
 * The default series priority, which picks the series with the highest
 * maximum values.
 */
const defaultSeriesPriority: Series.Priority = ([_series, data]) =>
  Math.max(...data.filter((y) => y !== null && y !== undefined));

/**
 * A helper function that computes the pointwise sum of series data.
 */
const pointwiseSum = (seriesData: Series.Data[]): Series.Data =>
  zipWith((xs) => sum(xs.filter((x) => x !== null)), seriesData);

/**
 * Convert an optional value to a list to support unpacking.
 */
function maybeToArray<X>(x?: X): X[] {
  return x === undefined ? [] : [x];
}

/**
 * The default scales used by this plot.
 */
const defaultScales: uPlot.Scales = {
  x: { time: true },
  y: { time: false },
};

/**
 * The default cursor sync matches.
 */
const cursorSyncMatch: uPlot.Cursor.Sync.ScaleKeyMatcher = (x, y) => x === y;

/**
 * Create an uPlot cursor sync configuration from a key.
 */
const asCursorSync = (
  cursorSyncPubSub: uPlot.SyncPubSub | null,
): Pick<uPlot.Cursor, "sync"> =>
  cursorSyncPubSub !== null
    ? {
        sync: {
          key: cursorSyncPubSub.key,
          match: [cursorSyncMatch, cursorSyncMatch],
        },
      }
    : {};

export default function Plot<PlotAttrs extends z.Attrs>({
  name,
  validateAttrs,
  getSeriesLabel,
  getSeriesOptions,
  unit,
  seriesLimit,
  seriesPriority = defaultSeriesPriority,
  seriesStyle = defaultSeriesStyle,
  totalSeries,
  otherSeries,
  options,
}: Props<PlotAttrs>) {
  // The current color scheme and a reference to the previous color scheme.
  const { mode, systemMode } = useColorScheme();
  const propModeRef = useRef(getMode(mode, systemMode));

  // A reference to the previous options.
  const propOptionsRef = useRef(options);

  // The formatter for the current unit.
  const formatter = unit && getFormatter(unit);
  const yAxisValues: Axis.Formatter = formatter
    ? {
        size: 75,
        values: (_self, values) => values.map((value) => formatter(value)),
      }
    : {};
  const seriesValue: Series.Formatter = formatter
    ? {
        value: (_self, value) => formatter(value, 2),
      }
    : {};

  // The context that provides us with export requests.
  const { lastExportRequest } = useContext(ExportRequestContext);

  // The context that provides us with the state of the refresh switch.
  const { refresh } = useContext(RefreshContext);

  // The context that provides us with our cursor sync group.
  const { cursorSync, cursorSyncPubSub } = useContext(CursorSyncContext);

  // Handle changes in cursorSync.
  useEffect(() => {
    if (plotRef.current === null) return;
    if (cursorSyncPubSub === null) return;
    if (cursorSync) {
      cursorSyncPubSub.sub(plotRef.current);
    } else {
      cursorSyncPubSub.unsub(plotRef.current);
    }
  }, [cursorSync]);

  // A reference to the remaining series styles.
  const seriesStyleRef = useRef<SeriesStyle[]>([...seriesStyle]);

  // References to the cached data.
  const cachedDataRef = useRef<AlignedData>([[]]);
  const cachedSeriesRef = useRef<Series>([{}]);
  const cachedIndicesRef = useRef<Series.Indices>({});

  // Compute the live data from the cached data.
  const getLiveView = useCallback((): [Series, AlignedData] => {
    const [cachedXSeries, ...cachedSeries] = cachedSeriesRef.current;
    const [cachedXValues, ...cachedSeriesData] = cachedDataRef.current;
    if (seriesLimit !== undefined && cachedSeries.length > seriesLimit) {
      // Sort the series and data by the priority...
      const [sortedYSeries, sortedSeriesData] = unzip(
        // Start from the cached series...
        cachedSeries
          // ...with the series data...
          .entries()
          .map(([i, a]): [uPlot.Series, Series.Data] => [
            a,
            cachedSeriesData[i],
          ])
          .toArray()
          // ...sorted according to the series priority in descending order...
          .toSorted((a, b) => seriesPriority(b) - seriesPriority(a)),
      );

      // Compute the live series and data...
      const liveYSeries = sortedYSeries.slice(0, seriesLimit);
      const liveSeriesData = sortedSeriesData.slice(0, seriesLimit);

      // Compute the other series data...
      const otherSeriesData =
        otherSeries && pointwiseSum(sortedSeriesData.slice(seriesLimit + 1));

      // Compute the total series data...
      const totalSeriesData =
        totalSeries &&
        pointwiseSum(
          // If we computed the other series, we can compute the total faster:
          otherSeriesData
            ? [otherSeriesData, ...liveSeriesData]
            : sortedSeriesData,
        );

      return [
        [
          cachedXSeries,
          ...maybeToArray(totalSeries),
          ...liveYSeries,
          ...maybeToArray(otherSeries),
        ],
        [
          cachedXValues,
          ...maybeToArray(totalSeriesData),
          ...liveSeriesData,
          ...maybeToArray(otherSeriesData),
        ],
      ];
    } else {
      // Compute the total series data...
      const totalSeriesData = totalSeries && pointwiseSum(cachedSeriesData);
      return [
        [cachedXSeries, ...maybeToArray(totalSeries), ...cachedSeries],
        [cachedXValues, ...maybeToArray(totalSeriesData), ...cachedSeriesData],
      ];
    }
  }, [seriesLimit]);

  // Add a new series to the cached data and return its index.
  const addSeries = useCallback(
    (label: string, options?: Series.Options): number => {
      const index = cachedSeriesRef.current.length;
      cachedIndicesRef.current[label] = index;
      cachedSeriesRef.current[index] = options ?? getSeriesOptions(label);
      cachedDataRef.current[index] = new Array(
        cachedDataRef.current[0].length,
      ).fill(null);
      return index;
    },
    [],
  );

  // If the given uPlot series does not have a stroke colour,
  // assign it the next style from the series styles.
  const asSeries = useCallback((series: uPlot.Series): uPlot.Series => {
    const seriesStyle =
      series.stroke === undefined ? seriesStyleRef.current.shift() : {};
    return { ...series, ...seriesStyle, ...seriesValue };
  }, []);

  // Resolve an axis stroke.
  const getStroke = useCallback(
    (stroke?: Axis.Stroke): uPlot.Axis.Stroke | undefined => {
      if (
        typeof stroke === "string" ||
        typeof stroke === "function" ||
        stroke instanceof CanvasGradient ||
        stroke instanceof CanvasPattern
      ) {
        return stroke;
      } else if (propModeRef.current === "dark") {
        return stroke?.dark ?? "#fff";
      } else {
        return stroke?.light ?? "#000";
      }
    },
    [],
  );

  const asAxis = useCallback(
    (axis: Axis, k: number): uPlot.Axis => ({
      // NOTE: This check avoids overwriting the x-axis formatter.
      ...(k > 0 ? yAxisValues : {}),
      ...axis,
      stroke: getStroke(axis.stroke),
    }),
    [],
  );

  // The plot and target element.
  const plotRef = useRef<uPlot | null>(null);
  const elemRef = useRef<HTMLDivElement>(null);

  // The placeholder element ID.
  const placeholderId = useId();

  // Create a new uPlot instance, if one does not exist.
  const create = useCallback(
    ([[xSeries, ...ySeries], data]: [Series, AlignedData]) => {
      if (plotRef.current !== null) return;

      // Remove the placeholder element.
      document.getElementById(placeholderId)?.remove();

      // Prepare the plot options.
      const series = [xSeries, ...ySeries.map(asSeries)];
      const pAxes: Axis[] = options.axes ?? new Array(2).fill({});
      const uAxes: uPlot.Axis[] = pAxes.map(asAxis);

      // Create the plot.
      plotRef.current = new uPlot(
        {
          ...options,
          cursor: asCursorSync(cursorSyncPubSub),
          scales: defaultScales,
          series,
          axes: uAxes,
        },
        data,
        elemRef.current as HTMLDivElement,
      );
    },
    [asAxis, asSeries],
  );

  // Destroy the current uPlot instance.
  const destroy = useCallback(() => {
    if (plotRef.current === null) return;
    plotRef.current.destroy();
    plotRef.current = null;
  }, []);

  // Update the uPlot instance with the latest data.
  const update = useCallback(
    (shouldCreate?: boolean) => {
      // Compute the live view.
      const newView = getLiveView();

      // If there is no instance...
      if (plotRef.current === null) {
        // ...create one.
        if (shouldCreate) create(newView);
      }

      // Otherwise, update the data for the current instance...
      else {
        const [[_newXSeries, ...newSeries], newData] = newView;
        const [_oldXSeries, ...oldSeries] = plotRef.current.series;

        // Find the first series that has a different label from the
        // corresponding series in the current plot...
        const firstChange = newSeries.find(
          (a, i) => a.label !== oldSeries.at(i)?.label,
        );

        // If the old and new series are the same...
        if (firstChange === undefined) {
          // ...set the new data.
          plotRef.current.setData(newData);
        }

        // If the old series are a prefix of the new series...
        else if (firstChange === oldSeries.length - 1) {
          // ...add the new series...
          for (let i = oldSeries.length; i < newSeries.length; ++i) {
            plotRef.current.addSeries(asSeries(newSeries[i]), i + 1);
          }
          // ...and set the new data.
          plotRef.current.setData(newData);
        }

        // Otherwise, recreate the plot...
        else {
          // NOTE: We could try deleting the old series after firstChange and
          //       then adding the new series. This may or may not be faster.
          destroy();
          create(newView);
        }
      }
    },
    [getLiveView, asSeries, create, destroy],
  );

  // Handle changes in the plot options.
  //
  // The current implementation handles changes in the width and height.
  useEffect(() => {
    if (plotRef.current === null) return;
    const { width: oldWidth, height: oldHeight } = propOptionsRef.current;
    const { width, height } = options;
    if (width !== oldWidth || height !== oldHeight) {
      plotRef.current.setSize({ width, height });
      propOptionsRef.current = options;
    }
  }, [options.width, options.height]);

  // Handle changes in the browser color scheme.
  useEffect(() => {
    const oldMode = propModeRef.current;
    const newMode = getMode(mode, systemMode);
    if (oldMode !== newMode) {
      propModeRef.current = newMode;
      destroy();
      update();
    }
  }, [update, destroy, mode, systemMode]);

  // Handle incoming export requests.
  useEffect(() => {
    if (lastExportRequest === null) return;
    const alignedData = lastExportRequest.telemetry[name];
    if (alignedData === null || alignedData === undefined) return;

    // Gather the new data.
    const newXValues = alignedData.xValues;
    const newSeriesData: Series.Data[] = [];
    alignedData.series.forEach(({ attrs, yValues }) => {
      if (validateAttrs(attrs)) {
        // Ensure the series has an index.
        const label = getSeriesLabel(attrs);
        const index = cachedIndicesRef.current[label] ?? addSeries(label);

        // Add the new series data to the list.
        //
        // NOTE: Use `index - 1` as newSeriesData only holds yValues.
        newSeriesData[index - 1] = yValues;
      } else {
        console.warn(`Unexpected attrs for series ${name} `, attrs);
      }
    });

    // Merge the new and the old data.
    const [oldXValues, ...oldSeriesData] = cachedDataRef.current;
    const xValues = [...oldXValues, ...alignedData.xValues];
    const seriesData: Series.Data[] = [];
    for (let i = 0; i < cachedSeriesRef.current.length - 1; ++i) {
      const oldYValues =
        i in oldSeriesData
          ? oldSeriesData[i]
          : new Array(oldXValues.length).fill(null);
      const newYValues =
        i in newSeriesData
          ? newSeriesData[i]
          : new Array(newXValues.length).fill(null);
      seriesData[i] = [...oldYValues, ...newYValues];
    }

    // Commit the merged data to the cache.
    cachedDataRef.current = [xValues, ...seriesData];

    // Update the live view of the cached data.
    if (refresh) update(true);
  }, [lastExportRequest, refresh, addSeries, create, destroy]);

  return (
    <Box
      sx={{
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        marginBottom: "1rem",
      }}
    >
      <div ref={elemRef}>
        {/* The placeholder element */}
        <Box id={placeholderId}>
          {options.title && <div className="u-title">{options.title}</div>}
          <Box sx={{ width: options.width, height: options.height }}>
            <Typography sx={{ textAlign: "center" }}>
              Waiting for data...
            </Typography>
          </Box>
        </Box>
      </div>
    </Box>
  );
}
