import { Attrs } from "./attrs";

export type MetricName =
  | "heapAllocated"
  | "heapSize"
  | "blocksSize"
  | "heapLive"
  | "memCurrent"
  | "memNeeded"
  | "memReturned"
  | "gcCopied"
  | "gcSlop"
  | "gcFragmentation"
  | "heapProfSample"
  | "capabilityUsage"
  | "productivity";

export interface SomeMetrics {
  type: "metric";
  name: MetricName;
  values: Array<Metric>;
}

export interface Metric {
  value: number;
  time_unix_nano?: number;
  start_time_unix_nano?: number;
  attrs?: Attrs;
}

function isMetric(value: any): value is Metric {
  return (
    typeof value === "object" &&
    value !== null &&
    Object.hasOwn(value, "value") &&
    typeof value.number === "number"
  );
}
