import Plot, { Props } from "@/components/plots/Plot";
import * as z from "@/util/z";

/* GC Copied */

const gcCopiedAttrs = z.strictObject({
  name: z.literal("gcCopied"),
  capNo: z.number(),
  gen: z.number(),
});

type GcCopiedAttrs = z.infer<typeof gcCopiedAttrs>;

export interface GcCopiedProps {
  options?: Partial<Props<GcCopiedAttrs>["options"]>;
}

export const GcCopiedPlot = (props: GcCopiedProps) => (
  <Plot
    name="gcCopied"
    validateAttrs={(attrs) => z.validate(gcCopiedAttrs, attrs)}
    getSeriesLabel={(attrs) =>
      `GC Copied (cap ${attrs.capNo}, gen ${attrs.gen})`
    }
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "GC Copied",
      width: 600,
      height: 400,
      ...(props.options ?? {}),
    }}
  />
);

/* GC Slop */

const gcSlopAttrs = z.strictObject({
  name: z.literal("gcSlop"),
  capNo: z.number(),
  gen: z.number(),
});

type GcSlopAttrs = z.infer<typeof gcSlopAttrs>;

export interface GcSlopProps {
  options?: Partial<Props<GcSlopAttrs>["options"]>;
}

export const GcSlopPlot = (props: GcSlopProps) => (
  <Plot
    name="gcSlop"
    validateAttrs={(attrs) => z.validate(gcSlopAttrs, attrs)}
    getSeriesLabel={(attrs) => `GC Slop (cap ${attrs.capNo}, gen ${attrs.gen})`}
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "GC Slop",
      width: 600,
      height: 400,
      ...(props.options ?? {}),
    }}
  />
);

/* GC Fragmentation */

const gcFragmentationAttrs = z.strictObject({
  name: z.literal("gcFragmentation"),
  capNo: z.number(),
  gen: z.number(),
});

type GcFragmentationAttrs = z.infer<typeof gcFragmentationAttrs>;

export interface GcFragmentationProps {
  options?: Partial<Props<GcFragmentationAttrs>["options"]>;
}

export const GcFragmentationPlot = (props: GcFragmentationProps) => (
  <Plot
    name="gcFragmentation"
    validateAttrs={(attrs) => z.validate(gcFragmentationAttrs, attrs)}
    getSeriesLabel={(attrs) =>
      `GC Fragmentation (cap ${attrs.capNo}, gen ${attrs.gen})`
    }
    getSeriesOptions={(label) => ({ label, spanGaps: true })}
    unit="By"
    options={{
      title: "GC Fragmentation",
      width: 600,
      height: 400,
      ...(props.options ?? {}),
    }}
  />
);
