import tinycolor from "tinycolor2";

// This type represents the styling options for a series.
export interface SeriesStyle {
  stroke: string;
  dash: number[];
}

// This is matplotlib's default colour property cycle.
// See: https://matplotlib.org/stable/release/prev_whats_new/dflt_style_changes.html#colors-in-default-property-cycle
const basicSeriesStroke: Array<string> = [
  "#1f77b4",
  "#ff7f0e",
  "#2ca02c",
  "#d62728",
  "#9467bd",
  "#8c564b",
  "#e377c2",
  "#7f7f7f",
  "#bcbd22",
  "#17becf",
];

// Lighten a colour.
const lighten = (color: string, amount: number) =>
  tinycolor(color).lighten(amount).toString();

// Darken a colour.
const darken = (color: string, amount: number) =>
  tinycolor(color).darken(amount).toString();

// This is a basic series of dash styles.
const basicSeriesDash: Array<number[]> = [
  [], // —————
  [1, 1], // .....
  [10, 10], // - - -
  [20, 5], // -.-.-
];

// This combines the basic color and dash cycles.
const basicSeriesStyle: SeriesStyle[] = basicSeriesDash.flatMap(
  (dash: number[]) =>
    basicSeriesStroke.map((stroke: string) => ({ stroke, dash })),
);

// This combines the basic series style with two cycles with lighter and darker colours.
const defaultSeriesStyle: SeriesStyle[] = [
  ...basicSeriesStyle,
  ...basicSeriesStyle.map(({ stroke, dash }) => ({
    stroke: lighten(stroke, 20),
    dash,
  })),
  ...basicSeriesStyle.map(({ stroke, dash }) => ({
    stroke: darken(stroke, 20),
    dash,
  })),
];

export default defaultSeriesStyle;
