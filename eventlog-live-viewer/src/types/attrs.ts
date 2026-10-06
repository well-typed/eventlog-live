export type Attrs = Record<string, any>;

export function isAttrs(value: any): value is Attrs {
  return typeof value === "object" && value !== null;
}
