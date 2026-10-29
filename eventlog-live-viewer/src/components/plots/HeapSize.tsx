import Plot, { Props } from "@/components/plots/Plot";
import * as z from "@/util/z";

const heapSizeAttrs = z.strictObject({
  name: z.union([
    z.literal("heapSize"),
    z.literal("blocksSize"),
    z.literal("heapLive"),
  ]),
  capNo: z.number(),
});

type HeapSizeAttrs = z.infer<typeof heapSizeAttrs>;

export interface HeapSizeProps {
  options?: Partial<Props<HeapSizeAttrs>["options"]>;
}

export const HeapSizePlot = (props: HeapSizeProps) => (
  <Plot
    name="heapSize"
    validateAttrs={(attrs) => z.validate(heapSizeAttrs, attrs)}
    getSeriesLabel={(attrs) =>
      `${camelToTitle(attrs.name)} (cap ${attrs.capNo})`
    }
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "Heap Size",
      width: 600,
      height: 400,
      ...(props.options ?? {}),
    }}
  />
);

const camelToTitle = (str: string): string =>
  str.charAt(0).toUpperCase() + str.slice(1).replaceAll(/([A-Z]+)/g, " $1");
