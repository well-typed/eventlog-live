"use client";

import Plot from "@/components/plots/Plot";
import * as z from "@/util/z";

/* GC Copied */

const gcCopiedAttrs = z.strictObject({
  name: z.literal("gcCopied"),
  capNo: z.number(),
  gen: z.number(),
});

export const GcCopiedPlot = () => (
  <Plot
    name="gcCopied"
    validateAttrs={(attrs) => z.validate(gcCopiedAttrs, attrs)}
    getSeriesLabel={(attrs) => `cap ${attrs.capNo}, gen ${attrs.gen}`}
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "GC Copied",
      width: 600,
      height: 400,
      scales: {
        x: { time: true },
        y: { time: false },
      },
    }}
  />
);

/* GC Slop */

const gcSlopAttrs = z.strictObject({
  name: z.literal("gcSlop"),
  capNo: z.number(),
  gen: z.number(),
});

export const GcSlopPlot = () => (
  <Plot
    name="gcSlop"
    validateAttrs={(attrs) => z.validate(gcSlopAttrs, attrs)}
    getSeriesLabel={(attrs) => `cap ${attrs.capNo}, gen ${attrs.gen}`}
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "GC Slop",
      width: 600,
      height: 400,
      scales: {
        x: { time: true },
        y: { time: false },
      },
    }}
  />
);

/* GC Fragmentation */

const gcFragmentationAttrs = z.strictObject({
  name: z.literal("gcFragmentation"),
  capNo: z.number(),
  gen: z.number(),
});

export const GcFragmentationPlot = () => (
  <Plot
    name="gcFragmentation"
    validateAttrs={(attrs) => z.validate(gcFragmentationAttrs, attrs)}
    getSeriesLabel={(attrs) => `cap ${attrs.capNo}, gen ${attrs.gen}`}
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    options={{
      title: "GC Fragmentation",
      width: 600,
      height: 400,
      scales: {
        x: { time: true },
        y: { time: false },
      },
    }}
  />
);
