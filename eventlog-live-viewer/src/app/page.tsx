"use client";

import { useEffect, useRef, useState } from "react";
import styles from "./page.module.css";
import UplotReact from "uplot-react";
import "uplot/dist/uPlot.min.css";

interface Options {
  url: string;
  options: uPlot.Options;
  target?: HTMLElement;
  onDelete?: (chart: uPlot) => void;
  onCreate?: (chart: uPlot) => void;
  resetScales?: boolean;
  className?: string;
}

interface NumberDataPoint {
  timestamp: number;
  value: number;
}

const isNumberDataPoint = (measure: any): measure is NumberDataPoint =>
  typeof measure === "object" &&
  Object.hasOwn(measure, "timestamp") &&
  Object.hasOwn(measure, "value");

const addNumberDataPoint = (
  data: uPlot.AlignedData,
  measure: NumberDataPoint,
): uPlot.AlignedData => [
  [...data[0], measure.timestamp],
  [...data[1], measure.value],
];

function Plot({ url, options }: Options) {
  const wsRef = useRef<WebSocket | null>(null);
  const [data, setData] = useState<uPlot.AlignedData>([[], []]);
  const [messages, setMessages] = useState<string[]>([]);

  useEffect(() => {
    const socket = new WebSocket(url);
    wsRef.current = socket;

    socket.onmessage = (event) => {
      console.log(event);
      setMessages((old) => [...old, event.data]);
      // try {
      //   const measure = JSON.parse(event.data);
      //   if (isNumberDataPoint(measure)) {
      //     console.debug(`Measure: ${JSON.stringify(measure)}`);
      //     return setData((oldData) => addNumberDataPoint(oldData, measure));
      //   } else {
      //     console.error(`Malformed Message: ${measure}`);
      //   }
      // } catch (e) {
      //   console.error(`Syntax Error: ${e}`);
      // }
    };

    socket.onerror = (event) => {
      console.error(`WebSocket Error: ${event}`);
    };

    return () => {
      socket.close(1000, "Component unmounted.");
    };
  }, []);

  // if (wsRef.current?.readyState === WebSocket.OPEN || wsRef.current?.readyState === WebSocket.CONNECTING) {
  //   return <UplotReact data={data} options={options} />;
  // } else {
  //   return <div>Not connected.</div>;
  // }
  return (
    <div>
      <h1>Messages</h1>
      <ul>
        {messages.map((message) => (
          <li>{message}</li>
        ))}
      </ul>
    </div>
  );
}

const options: uPlot.Options = {
  width: 800,
  height: 600,
  scales: {
    x: {
      time: true,
    },
    y: {
      time: false,
      range: [-100, 100],
    },
  },
  axes: [{}],
  series: [
    {},
    {
      stroke: "blue",
    },
  ],
};

export default function Home() {
  return (
    <div className={styles.page}>
      <main className={styles.main}>
        <Plot url="ws://127.0.0.1:30180" options={options} />
      </main>
    </div>
  );
}
