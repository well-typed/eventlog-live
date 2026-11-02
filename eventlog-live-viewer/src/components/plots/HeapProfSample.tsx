import Plot, { Props, Series, SeriesLimitInput } from "@/components/plots/Plot";
import * as z from "@/util/z";
import { Box, Toolbar, useTheme } from "@mui/material";
import useMediaQuery from "@mui/material/useMediaQuery";
import { useEffect, useState } from "react";

const heapProfSampleByClosureTypeAttrs = z.strictObject({
  name: z.literal("heapProfSample"),
  heapProfBreakdown: z.literal("-hT"),
  heapProfId: z.literal(0),
  heapProfLabel: z.string(),
  heapProfSampleEra: z.number(),
});

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

const heapProfSampleAttrs = z.union([
  heapProfSampleByClosureTypeAttrs,
  heapProfSampleByInfoProvAttrs,
]);

type HeapProfSampleAttrs = z.infer<typeof heapProfSampleAttrs>;

export interface HeapProfSampleProps {
  seriesLimit?: Props<HeapProfSampleAttrs>["seriesLimit"];
  options?: Omit<Props<HeapProfSampleAttrs>["options"], "width" | "height">;
  size?: (lg: boolean, xl: boolean) => [number, number];
}

const defaultSize = (lg: boolean, xl: boolean): [number, number] =>
  xl ? [1200, 800] : lg ? [900, 600] : [600, 400];

const getSeriesLabel = (attrs: HeapProfSampleAttrs): string =>
  attrs.heapProfBreakdown === "-hT"
    ? attrs.heapProfLabel
    : `${attrs.ipModule}:${attrs.ipLabel}`;

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

export const HeapProfSamplePlot = (props: HeapProfSampleProps) => {
  const theme = useTheme();
  const lg = useMediaQuery(theme.breakpoints.up("lg"));
  const xl = useMediaQuery(theme.breakpoints.up("xl"));
  const getSize = props.size ?? defaultSize;
  const [size, setSize] = useState<[number, number]>(getSize(lg, xl));
  const totalSeries = { label: "Total" };
  const otherSeries = { label: "Other" };
  const [seriesLimit, setSeriesLimit] = useState(props.seriesLimit ?? 10);

  // Handle changes in the viewport size.
  useEffect(() => setSize(getSize(lg, xl)), [lg, xl]);

  return (
    <>
      <Toolbar disableGutters variant="dense">
        <Box sx={{ flexGrow: 1 }} />
        <SeriesLimitInput
          defaultValue={seriesLimit}
          otherLabel={otherSeries.label}
          setSeriesLimit={setSeriesLimit}
        />
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
          title: "Heap Profile",
          width: size[0],
          height: size[1],
          ...(props.options ?? {}),
        }}
      />
    </>
  );
};
