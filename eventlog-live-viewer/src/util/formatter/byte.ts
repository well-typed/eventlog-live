import { Formatter } from "../formatter";

const byteBaseAndUnit = [
  "B",
  "KB",
  "MB",
  "GB",
  "TB",
  "PB",
  "EB",
  "ZB",
  "YB",
  "RB",
  "QB",
]
  .entries()
  .map(([exponent, unit]): [number, string] => [1000 ** exponent, unit])
  .toArray()
  .reverse();

const byteFormatter: Formatter = (value, precision) => {
  if (value === null) {
    return "--";
  } else {
    const absBytes = Math.abs(value);
    for (const [base, unit] of byteBaseAndUnit) {
      if (absBytes >= base)
        return `${(value / base).toFixed(precision ?? 0)} ${unit}`;
    }
    return `${value} B`;
  }
};

export default byteFormatter;
