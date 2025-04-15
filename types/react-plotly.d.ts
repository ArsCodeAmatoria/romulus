declare module 'react-plotly.js' {
  import * as React from 'react';
  
  interface PlotParams {
    data?: Array<any>;
    layout?: any;
    frames?: Array<any>;
    config?: any;
    useResizeHandler?: boolean;
    style?: React.CSSProperties;
    className?: string;
    onInitialized?: (figure: any, graphDiv: any) => void;
    onUpdate?: (figure: any, graphDiv: any) => void;
    onPurge?: (figure: any, graphDiv: any) => void;
    onError?: (err: any) => void;
    onAfterPlot?: (figure: any, graphDiv: any) => void;
    onRedraw?: (figure: any, graphDiv: any) => void;
    onSelected?: (figure: any, graphDiv: any) => void;
    onSelecting?: (figure: any, graphDiv: any) => void;
    onRestyle?: (figure: any, graphDiv: any) => void;
    onRelayout?: (figure: any, graphDiv: any) => void;
    onClickAnnotation?: (figure: any, graphDiv: any) => void;
    onLegendClick?: (figure: any, graphDiv: any) => boolean;
    onLegendDoubleClick?: (figure: any, graphDiv: any) => boolean;
    onSliderChange?: (figure: any, graphDiv: any) => void;
    onSliderEnd?: (figure: any, graphDiv: any) => void;
    onSliderStart?: (figure: any, graphDiv: any) => void;
    onAnimated?: (figure: any, graphDiv: any) => void;
    onAnimatingFrame?: (figure: any, graphDiv: any) => void;
    onAnimationInterrupted?: (figure: any, graphDiv: any) => void;
    onAutoSize?: (figure: any, graphDiv: any) => void;
    onDeselect?: (figure: any, graphDiv: any) => void;
    onDoubleClick?: (figure: any, graphDiv: any) => void;
    onFramework?: (figure: any, graphDiv: any) => void;
    onHover?: (figure: any, graphDiv: any) => void;
    onTransitioning?: (figure: any, graphDiv: any) => void;
    onTransitionInterrupted?: (figure: any, graphDiv: any) => void;
    onUnhover?: (figure: any, graphDiv: any) => void;
    [key: string]: any;
  }

  const Plot: React.ComponentClass<PlotParams>;
  export default Plot;
} 