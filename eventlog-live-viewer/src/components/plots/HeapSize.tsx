"use client";

import Plot from "@/components/plots/Plot";
import * as z from "@/util/z";

const heapSizeAttrs = z.strictObject({
  name: z.union([
    z.literal("heapSize"),
    z.literal("blocksSize"),
    z.literal("heapLive"),
  ]),
  capNo: z.number(),
});

export const HeapSizePlot = () => (
  <Plot
    name="heapSize"
    validateAttrs={(attrs) => z.validate(heapSizeAttrs, attrs)}
    getSeriesLabel={(attrs) => `${attrs.name}, cap ${attrs.capNo}`}
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "Heap Size",
      width: 600,
      height: 400,
      scales: {
        x: { time: true },
        y: { time: false },
      },
    }}
  />
);
