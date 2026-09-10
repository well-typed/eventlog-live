import * as z from "zod/mini";
export * from "zod/mini";

/* Resource */

export type Resource = z.infer<typeof resource>;

export const resource = z.partialRecord(z.string(), z.any());

/* Instrumentation Scope */

export type InstrumentationScope = z.infer<typeof instrumentationScope>;

export const instrumentationScope = z.strictObject({
  name: z.string(),
  version: z.string(),
});

/* Attributes */

export type Attrs = z.infer<typeof attrs>;

export const attrs = z.partialRecord(z.string(), z.any());

/* Series Attributes */

export type SeriesAttrs = z.infer<typeof seriesAttrs>;

export const seriesAttrs = z.catchall(
  z.strictObject({ name: z.string() }),
  z.any(),
);

/* Series */

export type Series = z.infer<typeof series>;

export const series = z.strictObject({
  attrs: seriesAttrs,
  yValues: z.array(z.nullable(z.number())),
});

/* AlignedData */

export type AlignedData = z.infer<typeof alignedData>;

export const alignedData = z.strictObject({
  xValues: z.array(z.number()),
  series: z.array(series),
});

/* ViewerTelemetry */

export type ViewerTelemetry = z.infer<typeof viewerTelemetry>;

export const viewerTelemetry = z.partialRecord(
  z.literal(["heapSize", "gcCopied", "gcSlop", "gcFragmentation"]),
  z.nullable(alignedData),
);

/* ViewerExportRequest */

export type ViewerExportRequest = z.infer<typeof viewerExportRequest>;

export const viewerExportRequest = z.strictObject({
  resource: resource,
  scope: instrumentationScope,
  telemetry: viewerTelemetry,
});
