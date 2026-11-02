import Plot, { Props, Series } from "@/components/plots/Plot";
import * as z from "@/util/z";
import Box from "@mui/material/Box";
import { useTheme } from "@mui/material/styles";
import Toolbar from "@mui/material/Toolbar";
import useMediaQuery from "@mui/material/useMediaQuery";
import { useEffect, useState } from "react";
import ControlButton from "../ControlButton";
import SeriesLimitInput from "./SeriesLimitInput";
import Typography from "@mui/material/Typography";
import Grid from "@mui/material/Grid";

/**
 * The zod type for the attributes for an -hT heap profile sample.
 */
const heapProfSampleByClosureTypeAttrs = z.strictObject({
  name: z.literal("heapProfSample"),
  heapProfBreakdown: z.literal("-hT"),
  heapProfId: z.literal(0),
  heapProfLabel: z.string(),
  heapProfSampleEra: z.number(),
});

/**
 * The zod type for the attributes for an -hi heap profile sample.
 */
const heapProfSampleByInfoProvAttrs = z.strictObject({
  name: z.literal("heapProfSample"),
  heapProfBreakdown: z.literal("-hi"),
  heapProfId: z.literal(0),
  heapProfLabel: z.string(),
  heapProfSampleEra: z.number(),
  ipClosureDesc: z.number(),
  ipLabel: z.string(),
  ipModule: z.string(),
  ipName: z.string(),
  ipSrcLoc: z.string(),
  ipTyDesc: z.string(),
});

/**
 * The zod type for the attributes for a heap profile sample.
 */
const heapProfSampleAttrs = z.union([
  heapProfSampleByClosureTypeAttrs,
  heapProfSampleByInfoProvAttrs,
]);

/**
 * The type of the attributes for a heap profile sample.
 */
type HeapProfSampleAttrs = z.infer<typeof heapProfSampleAttrs>;

/**
 * The properties for the HeapProfSamplePlot component.
 */
export interface HeapProfSampleProps {
  seriesLimit?: Props<HeapProfSampleAttrs>["seriesLimit"];
  options?: Omit<Props<HeapProfSampleAttrs>["options"], "width" | "height">;
  size?: (lg: boolean, xl: boolean) => [number, number];
}

/**
 * Get the series label from the heap profile sample attributes.
 */
const getSeriesLabel = (attrs: HeapProfSampleAttrs): string =>
  attrs.heapProfBreakdown === "-hT"
    ? attrs.heapProfLabel
    : `${attrs.ipModule}:${attrs.ipLabel}`;

/**
 * Get the series options from the heap profile sample label and attributes.
 */
const getSeriesOptions = (
  label: string,
  attrs: HeapProfSampleAttrs,
): Series.Options => {
  const spanGaps = true;
  if (attrs.heapProfBreakdown === `-hT`) {
    return { label, spanGaps };
  } else {
    const labelElement = document.createElement("span");
    labelElement.innerText = label;
    labelElement.title = attrs.ipSrcLoc;
    return { label: labelElement, spanGaps };
  }
};

/**
 * The plot for heap profile samples.
 */
export function HeapProfSamplePlot(props: HeapProfSampleProps) {
  const theme = useTheme();
  const lg = useMediaQuery(theme.breakpoints.up("lg"));
  const xl = useMediaQuery(theme.breakpoints.up("xl"));
  const getSize = props.size ?? (() => [600, 400]);
  const [size, setSize] = useState<[number, number]>(getSize(lg, xl));
  const totalSeries = { label: "Total" };
  const otherSeries = { label: "Other" };
  const [seriesLimit, setSeriesLimit] = useState(props.seriesLimit ?? 10);

  // Handle changes in the viewport size.
  useEffect(() => setSize(getSize(lg, xl)), [lg, xl]);

  return (
    <>
      <Toolbar
        disableGutters
        variant="dense"
        sx={{
          flexGrow: 1,
          justifyContent: "center",
        }}
      >
        <Grid
          container
          columns={10}
          sx={{
            width: size[0],
            alignItems: "center",
          }}
        >
          <Grid
            key={0}
            size={4}
            sx={{ display: "flex", justifyContent: "left" }}
          >
            <SeriesLimitInput
              defaultValue={seriesLimit}
              otherLabel={otherSeries.label}
              setSeriesLimit={setSeriesLimit}
            />
          </Grid>
          <Grid key={2} size={2} sx={{ textAlign: "center" }}>
            <div className="u-title">Heap&nbsp;Profile</div>
          </Grid>
          <Grid
            key={3}
            size={4}
            container
            columns={3}
            sx={{ display: "flex", justifyContent: "right" }}
          >
            <Grid
              key={31}
              size={1}
              sx={{ display: "flex", justifyContent: "center" }}
            >
              <ControlButton
                label="Start"
                title="Start heap profiling."
                namespace="eventlog-socket"
                commandId="start-heap-profiling"
              />
            </Grid>
            <Grid
              key={32}
              size={1}
              sx={{ display: "flex", justifyContent: "center" }}
            >
              <ControlButton
                label="Stop"
                title="Stop heap profiling."
                namespace="eventlog-socket"
                commandId="stop-heap-profiling"
              />
            </Grid>
            <Grid
              key={33}
              size={1}
              sx={{ display: "flex", justifyContent: "center" }}
            >
              <ControlButton
                label="Census"
                title="Request a single heap census."
                namespace="eventlog-socket"
                commandId="request-heap-census"
              />
            </Grid>
          </Grid>
        </Grid>
      </Toolbar>
      <Plot
        name="heapProfSample"
        validateAttrs={(attrs) => z.validate(heapProfSampleAttrs, attrs)}
        getSeriesLabel={getSeriesLabel}
        getSeriesOptions={getSeriesOptions}
        unit="By"
        seriesLimit={seriesLimit}
        totalSeries={totalSeries}
        otherSeries={otherSeries}
        options={{
          width: size[0],
          height: size[1],
          ...(props.options ?? {}),
        }}
      />
    </>
  );
}
