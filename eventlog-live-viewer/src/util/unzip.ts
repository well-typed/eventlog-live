/**
 * Unzip an array of pairs into a pair of arrays.
 *
 * @param xys An array of pairs.
 * @returns A pair of array.
 */
export default function unzip<X, Y>(xys: [X, Y][]) {
  return xys.reduce(
    ([xs, ys]: [X[], Y[]], [x, y]: [X, Y]): [X[], Y[]] => [
      [...xs, x],
      [...ys, y],
    ],
    [[], []],
  );
}
