import {
  createContext,
  PropsWithChildren,
  SyntheticEvent,
  useContext,
  useEffect,
  useState,
} from "react";
import Tab from "@mui/material/Tab";
import { ExportRequestContext } from "./ExportRequestProvider";
import * as z from "../util/z";
import TabContext from "@mui/lab/TabContext";
import TabList from "@mui/lab/TabList";
import TabPanel from "@mui/lab/TabPanel";
import { Typography } from "@mui/material";

/******************************************************************************/
/* Service Context                                                            */
/******************************************************************************/

export interface ServiceContext {
  name: z.Service.Name;
  instanceId: z.Service.InstanceId;
}

export const ServiceContext = createContext<ServiceContext>({
  name: z.Service.UNKNOWN_SERVICE_NAME,
  instanceId: z.Service.UNKNOWN_SERVICE_INSTANCE_ID,
});

/******************************************************************************/
/* Service Instance Tabs                                                      */
/******************************************************************************/

type ServiceInstanceExportRequests = Record<
  z.Service.InstanceId,
  z.ViewerExportRequest | null
>;

interface ServiceInstanceTabsProps {
  name: z.Service.Name;
}

function ServiceInstanceTabs({
  name,
  children,
}: PropsWithChildren<ServiceInstanceTabsProps>) {
  // The context that provides us with export requests.
  const { lastExportRequest } = useContext(ExportRequestContext);

  // The set of known services.
  const [instances, setInstances] = useState<ServiceInstanceExportRequests>({});

  // The selected tab.
  const [value, setValue] = useState<z.Service.InstanceId | null>(null);

  // Handle a tab change.
  const handleChange = (_event: SyntheticEvent, newValue: string) =>
    setValue(newValue);

  useEffect(() => {
    if (lastExportRequest === null) return;
    const { resource } = lastExportRequest;
    if (z.validate(z.service, resource)) {
      // If the export request is not for this service, ignore it...
      if (name !== z.Service.name(resource)) return;
      const instanceId = z.Service.instanceId(resource);

      // Update the export request for this service...
      setInstances((old) => ({ ...old, [instanceId]: lastExportRequest }));

      // If this is the first service, select it.
      if (value === null) setValue(instanceId);
    } else {
      console.warn(`Unexpected resource `, resource);
    }
  }, [lastExportRequest]);

  return value === null ? (
    <Typography>Waiting for data...</Typography>
  ) : (
    <TabContext value={value}>
      <TabList
        onChange={handleChange}
        variant="scrollable"
        scrollButtons="auto"
        aria-label="Tabs for the service instances"
        sx={{ borderBottom: 1, borderColor: "divider" }}
      >
        {Object.keys(instances).map((instanceId) => (
          <Tab label={instanceId} value={instanceId} />
        ))}
      </TabList>
      {Object.entries(instances).map(([instanceId, lastExportRequest]) => (
        <TabPanel
          value={instanceId}
          keepMounted={true}
          sx={{ paddingLeft: 0, paddingRight: 0 }}
        >
          <ServiceContext.Provider value={{ name, instanceId }}>
            <ExportRequestContext.Provider value={{ lastExportRequest }}>
              {children}
            </ExportRequestContext.Provider>
          </ServiceContext.Provider>
        </TabPanel>
      ))}
    </TabContext>
  );
}

/******************************************************************************/
/* Service Tabs                                                               */
/******************************************************************************/

type ServiceExportRequests = Record<
  z.Service.Name,
  z.ViewerExportRequest | null
>;

const eventlogLiveServiceNamePrefix = "eventlog-live-otlp-for-";

interface ServiceTabsProps {}

export default function ServiceTabs({
  children,
}: PropsWithChildren<ServiceTabsProps>) {
  // The context that provides us with export requests.
  const { lastExportRequest } = useContext(ExportRequestContext);

  // The set of known services.
  const [services, setServices] = useState<ServiceExportRequests>({});

  // The selected tab.
  const [value, setValue] = useState<z.Service.Name | null>(null);

  // Handle a tab change.
  const handleChange = (_event: SyntheticEvent, newValue: string) =>
    setValue(newValue);

  useEffect(() => {
    if (lastExportRequest === null) return;
    const { resource } = lastExportRequest;
    if (z.validate(z.service, resource)) {
      const name = z.Service.name(resource);

      // Filter out Eventlog Live services, as these don't send standard data:
      if (name.startsWith(eventlogLiveServiceNamePrefix)) return;

      // Update the export request for this service...
      setServices((old) => ({ ...old, [name]: lastExportRequest }));

      // If this is the first service, select it.
      if (value === null) setValue(name);
    } else {
      console.warn(`Unexpected resource `, resource);
    }
  }, [lastExportRequest]);

  return value === null ? (
    <Typography>Waiting for data...</Typography>
  ) : (
    <TabContext value={value}>
      <TabList
        onChange={handleChange}
        variant="scrollable"
        scrollButtons="auto"
        aria-label="Tabs for the services"
        sx={{ borderBottom: 1, borderColor: "divider" }}
      >
        {Object.keys(services).map((name) => (
          <Tab label={name} value={name} />
        ))}
      </TabList>
      {Object.entries(services).map(([name, lastExportRequest]) => (
        <TabPanel
          value={name}
          keepMounted={true}
          sx={{ paddingTop: 0, paddingLeft: 0, paddingRight: 0 }}
        >
          <ExportRequestContext.Provider value={{ lastExportRequest }}>
            <ServiceInstanceTabs name={name}>{children}</ServiceInstanceTabs>
          </ExportRequestContext.Provider>
        </TabPanel>
      ))}
    </TabContext>
  );
}
