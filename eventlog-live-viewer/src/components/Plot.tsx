"use client";

import { useContext, useEffect, useState } from "react";
import { TelemetryContext } from "./TelemetryProvider";
import UplotReact from "uplot-react";
import { AlignedData, ViewerTelemetry } from "@/util/z";
import { RefreshContext } from "./RefreshProvider";
import { Breakpoint } from "@mui/material/styles";
import { Box, useMediaQuery } from "@mui/material";

export type Dimension =
  | number
  | ({ xs: number } & { [breakpoint in Breakpoint]?: number });

export interface PlotProps {
  name: keyof ViewerTelemetry;
  legend: Array<string>;
  options: uPlot.Options;
}

export type SeriesData = Array<number | null>;

export type PlotData = [xValues: Array<number>, ...yValues: Array<SeriesData>];

export default function Plot(props: PlotProps) {
  const { name: plotName, legend, options } = props;
  const telemetryContext = useContext(TelemetryContext);
  const refreshContext = useContext(RefreshContext);
  const initialData: PlotData = [
    [],
    ...new Array<SeriesData>(legend.length).fill([]),
  ];
  const [data, setData] = useState<PlotData>(initialData);
  const [live, setLive] = useState<PlotData>(data);

  useEffect(() => {
    const lastExportRequest = telemetryContext.lastExportRequest;
    if (lastExportRequest !== null) {
      const alignedData: AlignedData | null | undefined =
        lastExportRequest.telemetry[plotName];
      if (alignedData !== null && alignedData !== undefined) {
        const newYValues = new Array(legend.length).fill(null);
        alignedData.series.forEach((s) => {
          const seriesIndex = legend.indexOf(s.attrs.name);
          if (seriesIndex !== -1) {
            newYValues[seriesIndex] = s.yValues;
          } else {
            console.error(`Unexpected series ${plotName}:${s.attrs.name}`, s);
          }
        });
        setData(
          ([oldXValues, ...oldSeries]: PlotData): PlotData => [
            [...oldXValues, ...alignedData.xValues],
            ...oldSeries.map((oldYValues, i) => [
              ...oldYValues,
              ...newYValues[i],
            ]),
          ],
        );
      }
    }
    if (refreshContext.refresh) {
      setLive(data);
    }
  }, [refreshContext.refresh, telemetryContext.lastExportRequest]);

  return (
    <Box
      sx={{
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        marginBottom: "1rem",
      }}
    >
      <UplotReact
        key={plotName}
        options={options}
        data={live}
        resetScales={true}
      />
    </Box>
  );
}
