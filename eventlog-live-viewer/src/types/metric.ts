import { Attrs, isAttrs } from "./attrs";

export interface SomeMetrics {
  name: string;
  metrics: Array<Metric>;
}

export interface Metric {
  value: number;
  time_unix_nano?: number;
  start_time_unix_nano?: number;
  attrs?: Attrs;
}

export function isSomeMetrics(metrics: any): metrics is SomeMetrics {
  return (
    typeof metrics === "object" &&
    metrics !== null &&
    Object.hasOwn(metrics, "name") &&
    typeof metrics.name === "string" &&
    Object.hasOwn(metrics, "metrics") &&
    Array.isArray(metrics.metrics) &&
    metrics.metrics.every(isMetric)
  );
}

export function isMetric(metric: any): metric is Metric {
  return (
    typeof metric === "object" &&
    metric !== null &&
    Object.hasOwn(metric, "value") &&
    typeof metric.value === "number" &&
    (Object.hasOwn(metric, "time_unix_nano")
      ? typeof metric.time_unix_nano === "number"
      : true) &&
    (Object.hasOwn(metric, "start_time_unix_nano")
      ? typeof metric.start_time_unix_nano === "number"
      : true) &&
    (Object.hasOwn(metric, "attrs") ? isAttrs(metric.attrs) : true)
  );
}
