import { Formatter } from "../formatter";

/**
 * Format a number to a given number of digits.
 */
function format(n: number, digits: number): string {
  const ndigits = Math.floor(Math.max(0, Math.log10(Math.abs(n)))) + 1;
  const precision = Math.max(0, digits - ndigits);
  const nstring = n.toFixed(precision);
  return precision <= 0 ? nstring : nstring.replace(/\.?0+$/, "");
}

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

const byteFormatter: Formatter = (value) => {
  if (value === null) {
    return "--";
  } else {
    const absBytes = Math.abs(value);
    for (const [base, unit] of byteBaseAndUnit) {
      if (absBytes >= base) return `${format(value / base, 3)} ${unit}`;
    }
    return `${value} B`;
  }
};

export default byteFormatter;
