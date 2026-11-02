import Box from "@mui/material/Box";
import Input, { InputProps } from "@mui/material/Input";
import Tooltip from "@mui/material/Tooltip";
import Typography from "@mui/material/Typography";
import { useId } from "react";

export type SeriesLimitInputProps = Omit<InputProps, "type" | "onChange"> & {
  defaultValue: number;
  otherLabel?: string;
  setSeriesLimit: (seriesLimit: number) => void;
};

const seriesLimitInputTooltip = (otherLabel?: string): string => {
  const basic = "The maximum number of series to display in this plot.";
  const other =
    otherLabel && ` Additional series are summarised under ${otherLabel}.`;
  return basic + (other ?? "");
};

/**
 * An input field that can be used for the seriesLimit property.
 */
export default function SeriesLimitInput(props: SeriesLimitInputProps) {
  const { otherLabel, setSeriesLimit, id, sx, ...inputProps } = props;
  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const { value } = event.target;
    const seriesLimit = parseInt(value);
    if (Number.isNaN(seriesLimit)) {
      console.log("Invalid value for series limit: ", value);
    } else {
      setSeriesLimit(seriesLimit);
    }
  };
  const inputId = id ?? useId();
  return (
    <Box sx={{ display: "flex", alignItems: "baseline", columnGap: 1 }}>
      <Typography>Series&nbsp;Limit</Typography>
      <Tooltip title={seriesLimitInputTooltip(otherLabel)}>
        <Input
          disableUnderline
          type="number"
          margin="dense"
          sx={{ ...(sx ?? {}), width: "2.5em" }}
          onChange={handleChange}
          id={inputId}
          {...inputProps}
        />
      </Tooltip>
    </Box>
  );
}
