export type Attrs = Record<string, any>;

function isAttrs(value: any): value is Attrs {
  return typeof value === "object" && value !== null;
}
