import Plot, { Props } from "@/components/plots/Plot";
import * as z from "@/util/z";
import { useTheme } from "@mui/material";
import useMediaQuery from "@mui/material/useMediaQuery";
import { useEffect, useState } from "react";

const heapProfSampleByClosureTypeAttrs = z.strictObject({
  name: z.literal("heapProfSample"),
  heapProfBreakdown: z.literal("-hT"),
  heapProfId: z.literal(0),
  heapProfLabel: z.string(),
  heapProfSampleEra: z.number(),
});

const heapProfSampleAttrs = z.union([heapProfSampleByClosureTypeAttrs]);

type HeapProfSampleAttrs = z.infer<typeof heapProfSampleAttrs>;

export interface HeapProfSampleProps {
  seriesLimit?: Props<HeapProfSampleAttrs>["seriesLimit"];
  options?: Omit<Props<HeapProfSampleAttrs>["options"], "width" | "height">;
  size?: (lg: boolean, xl: boolean) => [number, number];
}

const defaultSize = (lg: boolean, xl: boolean): [number, number] =>
  xl ? [1200, 800] : lg ? [900, 600] : [600, 400];

export const HeapProfSamplePlot = (props: HeapProfSampleProps) => {
  const theme = useTheme();
  const lg = useMediaQuery(theme.breakpoints.up("lg"));
  const xl = useMediaQuery(theme.breakpoints.up("xl"));
  const getSize = props.size ?? defaultSize;
  const [size, setSize] = useState<[number, number]>(getSize(lg, xl));

  // Handle changes in the viewport size.
  useEffect(() => setSize(getSize(lg, xl)), [lg, xl]);

  return (
    <Plot
      name="heapProfSample"
      validateAttrs={(attrs) => z.validate(heapProfSampleAttrs, attrs)}
      getSeriesLabel={(attrs) => attrs.heapProfLabel}
      getSeriesOptions={(label) => ({
        label,
        spanGaps: true,
      })}
      unit="By"
      seriesLimit={props.seriesLimit ?? 10}
      totalSeries={{ label: "Total" }}
      otherSeries={{ label: "Other" }}
      options={{
        title: "Heap Profile",
        width: size[0],
        height: size[1],
        ...(props.options ?? {}),
      }}
    />
  );
};
