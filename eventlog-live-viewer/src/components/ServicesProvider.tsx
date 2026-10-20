"use client";

import {
  createContext,
  PropsWithChildren,
  useContext,
  useEffect,
  useState,
} from "react";
import * as z from "../util/z";
import { ExportRequestContext } from "./ExportRequestProvider";

export type ServiceName = string;
export type ServiceInstanceId = string;
export type Services = Partial<Record<ServiceName, Set<ServiceInstanceId>>>;

export interface ServicesContext {
  services: Services;
}

export const ServicesContext = createContext<ServicesContext>({
  services: {},
});

export interface ServicesProviderProps {}

export default function ServiceProvider({
  children,
}: PropsWithChildren<ServicesProviderProps>) {
  const telemetry = useContext(ExportRequestContext);
  const [services, setServices] = useState<Services>({});

  useEffect(() => {
    if (telemetry.lastExportRequest !== null) {
      const { resource } = telemetry.lastExportRequest;
      if (z.validate(z.service, resource)) {
        console.log(resource);
        setServices((old) => {
          const name = resource["service.name"] ?? "";
          const instanceId = resource["service.instance.id"] ?? "";
          const oldIdSet = old[name] ?? new Set();
          const newIdSet = oldIdSet.union(new Set([instanceId]));
          return { ...old, [name]: newIdSet };
        });
      }
    }
  }, [telemetry.lastExportRequest]);

  return (
    <ServicesContext.Provider value={{ services }}>
      {children}
    </ServicesContext.Provider>
  );
}
