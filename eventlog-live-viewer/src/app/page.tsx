"use client";

import { useEffect, useRef, useState } from "react";
import styles from "./page.module.css";
import UplotReact from "uplot-react";
import "uplot/dist/uPlot.min.css";
import useWebSocket, { ReadyState } from "react-use-websocket";

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
  const [messages, setMessages] = useState<MessageEvent<any>[]>([]);

  const { sendMessage, lastMessage, readyState } = useWebSocket(url, {
    onOpen: () => {
      console.log("opened");
    },
  });

  useEffect(() => {
    if (lastMessage !== null) {
      setMessages((old) => old.concat(lastMessage));
    }
  }, [lastMessage]);

  const connectionStatus = {
    [ReadyState.CONNECTING]: "Connecting",
    [ReadyState.OPEN]: "Open",
    [ReadyState.CLOSING]: "Closing",
    [ReadyState.CLOSED]: "Closed",
    [ReadyState.UNINSTANTIATED]: "Uninstantiated",
  }[readyState];

  return (
    <div>
      <h1>Message</h1>
      <h2>Status: {connectionStatus}</h2>
      <ul>
        {messages.map((message, index) => (
          <li key={index}>{message.data}</li>
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
