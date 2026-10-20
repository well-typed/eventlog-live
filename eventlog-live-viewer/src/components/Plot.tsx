"use client";

import { useContext, useEffect, useState } from "react";
import { ExportRequestContext } from "./ExportRequestProvider";
import UplotReact from "uplot-react";
import * as z from "@/util/z";
import { RefreshContext } from "./RefreshProvider";
import { Box, Typography } from "@mui/material";
import assert from "assert";

export type PlotData = [xValues: Array<number>, ...yValues: Array<SeriesData>];

export type PlotOptions = Omit<uPlot.Options, "series">;

export type SeriesData = Array<number | null>;

export type SeriesLabel = string;

export type SeriesIndex = number;

export type SeriesIndices = Partial<Record<SeriesLabel, SeriesIndex>>;

export interface PlotState {
  data: PlotData;
  options: uPlot.Options;
  indices: SeriesIndices;
}

export interface PlotProps<PlotAttrs extends z.Attrs> {
  name: keyof z.ViewerTelemetry;
  validate: (attrs: any) => attrs is PlotAttrs;
  labelFor: (attrs: PlotAttrs) => SeriesLabel;
  seriesFor: (
    label: SeriesLabel,
    index: number,
    attrs: PlotAttrs,
  ) => uPlot.Series;
  options: PlotOptions;
}

export default function Plot<PlotAttrs extends z.Attrs>(
  props: PlotProps<PlotAttrs>,
) {
  const {
    name,
    validate,
    labelFor,
    seriesFor,
    options: partOfInitialOptions,
  } = props;
  const exportRequestContext = useContext(ExportRequestContext);
  const refreshContext = useContext(RefreshContext);

  const initialData: PlotData = [[]];
  const initialOptions: uPlot.Options = {
    ...partOfInitialOptions,
    series: [{}],
  };
  const [cache, setCache] = useState<PlotState>({
    data: initialData,
    options: initialOptions,
    indices: {},
  });
  const [data, setData] = useState<PlotData>(initialData);
  const [options, setOptions] = useState<uPlot.Options>(initialOptions);

  useEffect(() => {
    const lastExportRequest = exportRequestContext.lastExportRequest;
    if (lastExportRequest === null) return;
    const alignedData = lastExportRequest.telemetry[name];
    if (alignedData === null || alignedData === undefined) return;

    // Initialise the next cache value.
    const newXValues = alignedData.xValues;
    const newSeriesData: SeriesData[] = [];
    const options = { ...cache.options, series: [...cache.options.series] };
    const indices = { ...cache.indices };

    // Process each series data.
    alignedData.series.forEach((series: z.Series) => {
      if (validate(series.attrs)) {
        const label = labelFor(series.attrs);
        let index = indices[label];
        if (index === undefined) {
          // NOTE: The first object in options.series represents the x-values,
          //       so we must subtract one to get an index into the y-values.
          index = options.series.length - 1;
          indices[label] = index;
          options.series[index + 1] = seriesFor(label, index, series.attrs);
        }
        newSeriesData[index] = series.yValues;
      } else {
        console.warn("Unexpected series with attrs ", series.attrs);
      }
    });

    // Compute the new data.
    const [oldXValues, ...oldSeriesData] = cache.data;
    const xValues = [...oldXValues, ...alignedData.xValues];
    const seriesData = [];
    for (let i = 0; i < options.series.length; ++i) {
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
    const data: PlotData = [xValues, ...seriesData];
    if (!seriesData.every((yValues) => xValues.length === yValues.length)) {
      console.error(`Plot ${name}: data corrupted`);
    }
    // Update the state.
    setCache({ data, options, indices });

    // Update the live state.
    if (refreshContext.refresh) {
      setData(data);
      setOptions(options);
    }
  }, [refreshContext.refresh, exportRequestContext.lastExportRequest]);

  return (
    <Box
      sx={{
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        marginBottom: "1rem",
      }}
    >
      {data.length > 1 ? (
        <UplotReact
          key={name}
          options={options}
          data={data}
          resetScales={true}
        />
      ) : (
        <Box
          sx={{
            width: options.width,
            height: options.height,
          }}
        >
          <div className="u-title">{options.title}</div>
        </Box>
      )}
    </Box>
  );
}
