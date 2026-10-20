import { PropsWithChildren, useContext } from "react";
import { ServicesContext } from "./ServicesProvider";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import Box from "@mui/material/Box";

export interface ServiceTabsProps {}

export default function ServiceTabs({
  children,
}: PropsWithChildren<ServiceTabsProps>) {
  const serviceContext = useContext(ServicesContext);
  return (
    <Box sx={{ width: "100%" }}>
      <Box sx={{ borderBottom: 1, borderColor: "divider" }}>
        <Tabs>
          {Object.entries(serviceContext.services).map(
            ([name, idSet], _index) => (
              <Tab label={name}></Tab>
            ),
          )}
        </Tabs>
      </Box>
    </Box>
  );
}
