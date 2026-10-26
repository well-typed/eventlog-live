"use client";

import { useCallback, useContext, useEffect, useId, useRef } from "react";
import { ExportRequestContext } from "../ExportRequestProvider";
import * as z from "@/util/z";
import { RefreshContext } from "../RefreshProvider";
import { Box, Typography, useMediaQuery } from "@mui/material";
import uPlot from "uplot";
import unzip from "@/util/unzip";
import defaultSeriesStyle, { SeriesStyle } from "@/util/seriesStyle";
import { useColorScheme } from "@mui/material/styles";

export type PlotData = [xValues: Array<number>, ...yValues: Array<SeriesData>];

export type PlotUnit = "By";

export type PlotOptions = Omit<uPlot.Options, "series" | "axes"> & {
  axes?: PlotAxis[];
};

export type PlotSeries = [xSeries: uPlot.Series, ...ySeries: uPlot.Series[]];

export type PlotAxisStroke =
  | uPlot.Axis.Stroke
  | { light: uPlot.Axis.Stroke; dark: uPlot.Axis.Stroke };

export type PlotAxis = Omit<uPlot.Axis, "stroke"> & { stroke?: PlotAxisStroke };

export type SeriesOptions = Omit<uPlot.Series, "stroke" | "dash">;

export type SeriesData = Array<number | null>;

export type SeriesIndices = Partial<Record<string, number>>;

export interface PlotProps<PlotAttrs extends z.Attrs> {
  readonly name: keyof z.ViewerTelemetry;
  readonly validateAttrs: (attrs: any) => attrs is PlotAttrs;
  readonly getSeriesLabel: (attrs: PlotAttrs) => string;
  readonly getSeriesOptions: (label: string, index: number) => SeriesOptions;
  readonly unit?: PlotUnit;
  readonly seriesLimit?: number;
  readonly totalSeries?: SeriesOptions;
  readonly otherSeries?: SeriesOptions;
  readonly seriesPriority?: SeriesPriority;
  readonly seriesStyle?: SeriesStyle[];
  readonly options: PlotOptions;
}

type SeriesPriority = ([series, data]: [SeriesOptions, SeriesData]) => number;

const defaultSeriesPriority: SeriesPriority = ([_series, data]) =>
  Math.max(...data.filter((y) => y !== null && y !== undefined));

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
}: PlotProps<PlotAttrs>) {
  // The color scheme.
  const { mode, systemMode } = useColorScheme();
  const propModeRef = useRef(getMode(mode, systemMode));

  // The references to various properties.
  const propOptionsRef = useRef(options);

  // The formatter.
  const formatter = unit && getFormatter(unit);
  const yAxisValues: Pick<uPlot.Axis, "size" | "values"> = formatter
    ? {
        size: 75,
        values: (_self, values) => values.map((value) => formatter(value)),
      }
    : {};
  const seriesValue: Pick<uPlot.Series, "value"> = formatter
    ? {
        value: (_self, value) => formatter(value, 2),
      }
    : {};

  // The external contexts that provide us with our data.
  const exportRequestContext = useContext(ExportRequestContext);
  const refreshContext = useContext(RefreshContext);

  // The data references.
  const dataRef = useRef<PlotData>([[]]);
  const seriesRef = useRef<PlotSeries>([{}]);
  const indicesRef = useRef<SeriesIndices>({});
  const seriesStyleRef = useRef<SeriesStyle[]>([...seriesStyle]);

  // Compute the live plot view from the cached plot data.
  const getLiveView = useCallback((): [PlotSeries, PlotData] => {
    const [cachedXSeries, ...cachedSeries] = seriesRef.current;
    const [cachedXValues, ...cachedSeriesData] = dataRef.current;
    if (seriesLimit !== undefined && cachedSeries.length > seriesLimit) {
      // Sort the series and data by the priority...
      const [sortedYSeries, sortedSeriesData] = unzip(
        // Start from the cached series...
        cachedSeries
          // ...with the series data...
          .entries()
          .map(([i, a]): [uPlot.Series, SeriesData] => [a, cachedSeriesData[i]])
          .toArray()
          // ...sorted according to the series priority in descending order...
          .toSorted((a, b) => seriesPriority(b) - seriesPriority(a)),
      );

      // Compute the live series and data...
      const liveYSeries = sortedYSeries.slice(0, seriesLimit);
      const liveSeriesData = sortedSeriesData.slice(0, seriesLimit);

      // Compute the other series data...
      const otherSeriesData =
        otherSeries && sumSeriesData(sortedSeriesData.slice(seriesLimit + 1));

      // Compute the total series data...
      const totalSeriesData =
        totalSeries &&
        sumSeriesData(
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
      const totalSeriesData = totalSeries && sumSeriesData(cachedSeriesData);

      return [
        [cachedXSeries, ...maybeToArray(totalSeries), ...cachedSeries],
        [cachedXValues, ...maybeToArray(totalSeriesData), ...cachedSeriesData],
      ];
    }
  }, [seriesLimit]);

  // Add a series.
  const addSeries = useCallback(
    (label: string, options?: SeriesOptions): number => {
      const index = seriesRef.current.length;
      indicesRef.current[label] = index;
      seriesRef.current[index] = options ?? getSeriesOptions(label, index);
      dataRef.current[index] = new Array(dataRef.current[0].length).fill(null);
      return index;
    },
    [],
  );

  // Ensure that a series has a style.
  const asSeries = useCallback((series: uPlot.Series): uPlot.Series => {
    const seriesStyle =
      series.stroke === undefined ? seriesStyleRef.current.shift() : {};
    return { ...series, ...seriesStyle, ...seriesValue };
  }, []);

  // Resolve an axis stroke.
  const getStroke = useCallback(
    (stroke?: PlotAxisStroke): uPlot.Axis.Stroke | undefined => {
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
    (axis: PlotAxis, k: number): uPlot.Axis => ({
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

  // Create the plot.
  const create = useCallback(
    ([[xSeries, ...ySeries], data]: [PlotSeries, PlotData]) => {
      // Remove the placeholder element.
      document.getElementById(placeholderId)?.remove();

      // Prepare the plot options.
      const series = [xSeries, ...ySeries.map(asSeries)];
      const plotAxes: PlotAxis[] = options.axes ?? new Array(2).fill({});
      const axes: uPlot.Axis[] = plotAxes.map(asAxis);

      // Create the plot.
      plotRef.current = new uPlot(
        { ...options, series, axes },
        data,
        elemRef.current as HTMLDivElement,
      );
    },
    [getStroke, asSeries],
  );

  // Create the plot.
  const destroy = useCallback(() => {
    if (plotRef.current !== null) {
      plotRef.current.destroy();
      plotRef.current = null;
    }
  }, []);

  // Commit the latest data to the plot.
  const update = useCallback(() => {
    const newView = getLiveView();
    if (plotRef.current === null) {
      create(newView);
    } else {
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
  }, [create, destroy]);

  // Handle changes in the options.
  useEffect(() => {
    if (plotRef.current !== null) {
      const { width: oldWidth, height: oldHeight } = propOptionsRef.current;
      const { width, height } = options;
      if (width !== oldWidth || height !== oldHeight) {
        plotRef.current.setSize({ width, height });
        propOptionsRef.current = options;
      }
    }
  }, [options.width, options.height]);

  // Handle changes in the color scheme.
  useEffect(() => {
    const oldMode = propModeRef.current;
    const newMode = getMode(mode, systemMode);
    if (oldMode !== newMode) {
      propModeRef.current = newMode;
      destroy();
      update();
    }
  }, [update, destroy, mode, systemMode]);

  // Handle new export requests.
  useEffect(() => {
    const lastExportRequest = exportRequestContext.lastExportRequest;
    if (lastExportRequest === null) return;
    const alignedData = lastExportRequest.telemetry[name];
    if (alignedData === null || alignedData === undefined) return;

    // Gather the new series data.
    const newXValues = alignedData.xValues;
    const newSeriesData: SeriesData[] = [];

    // Process each series data.
    alignedData.series.forEach(({ attrs, yValues }) => {
      if (validateAttrs(attrs)) {
        // Ensure the series has an index.
        const label = getSeriesLabel(attrs);
        const index = indicesRef.current[label] ?? addSeries(label);

        // Add the new series data to the list.
        newSeriesData[index - 1] = yValues;
      } else {
        console.warn(`Unexpected attrs for series ${name} `, attrs);
      }
    });

    // Commit the new series data to our dataRef.
    const [oldXValues, ...oldSeriesData] = dataRef.current;
    const xValues = [...oldXValues, ...alignedData.xValues];
    const seriesData: SeriesData[] = [];
    for (let i = 0; i < seriesRef.current.length - 1; ++i) {
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
    dataRef.current = [xValues, ...seriesData];

    // Commit our new series and data to our plot.
    if (refreshContext.refresh) update();
  }, [
    exportRequestContext.lastExportRequest,
    refreshContext.refresh,
    addSeries,
    asSeries,
    create,
    destroy,
  ]);

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

const getFormatter = (_unit: PlotUnit) => byteFormatter;

const byteBaseAndUnit = [
  "B",
  "KB",
  "MB",
  "GB",
  "TB",
  "PB",
  "EB",
  "ZB",
  "YB",
  "RB",
  "QB",
]
  .entries()
  .map(([exponent, unit]): [number, string] => [1000 ** exponent, unit])
  .toArray()
  .reverse();

export const byteFormatter = (
  bytes: number | null,
  precision?: number,
): string => {
  if (bytes === null) {
    return "--";
  } else {
    const absBytes = Math.abs(bytes);
    for (const [base, unit] of byteBaseAndUnit) {
      if (absBytes >= base)
        return `${(bytes / base).toFixed(precision ?? 0)} ${unit}`;
    }
    return `${bytes} B`;
  }
};

/**
 * Compute the pointwise sum of a series of data.
 */
function sumSeriesData(seriesData: SeriesData[]): SeriesData {
  return Array.apply(null, new Array(seriesData.at(0)?.length ?? 0)).map(
    (_, k) =>
      seriesData
        .map((yValues) => yValues[k])
        .filter((x) => x !== null)
        .reduce((acc, x) => acc + x),
  );
}

/**
 * Convert an optional value to a list to support unpacking.
 */
function maybeToArray<X>(x?: X): X[] {
  return x === undefined ? [] : [x];
}

/**
 * Get the effective mode from the mode and systemMode.
 */
function getMode(
  mode?: "light" | "dark" | "system",
  systemMode?: "light" | "dark",
): "light" | "dark" | undefined {
  return mode === "system" ? systemMode : mode;
}
