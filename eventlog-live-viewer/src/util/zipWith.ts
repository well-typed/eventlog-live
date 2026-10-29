/**
 * Zip an array of arrays with a custom operator.
 */
export default function zipWith<X>(op: (xs: X[]) => X, xss: X[][]): X[] {
  const n: number = xss.reduce((n, xs) => Math.min(n, xs.length), 0);
  return Array.apply(null, new Array(n)).map((_, k) =>
    op(xss.map((xs) => xs[k])),
  );
}
