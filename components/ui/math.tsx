'use client';

import { useEffect, useRef, createElement } from 'react';
import katex from 'katex';
import 'katex/dist/katex.min.css';

interface MathProps {
  math: string;
  display?: boolean;
  className?: string;
}

export function Math({ math, display = false, className = '' }: MathProps) {
  // Use separate refs for div and span elements
  const divRef = useRef<HTMLDivElement>(null);
  const spanRef = useRef<HTMLSpanElement>(null);
  
  useEffect(() => {
    const container = display ? divRef.current : spanRef.current;
    if (container) {
      katex.render(math, container, {
        displayMode: display,
        throwOnError: false,
      });
    }
  }, [math, display]);

  // Render the appropriate element based on display mode
  if (display) {
    return <div ref={divRef} className={className} />;
  } else {
    return <span ref={spanRef} className={className} />;
  }
}

export function InlineMath({ math, className = '' }: Omit<MathProps, 'display'>) {
  return <Math math={math} display={false} className={className} />;
}

export function DisplayMath({ math, className = '' }: Omit<MathProps, 'display'>) {
  return <Math math={math} display={true} className={className} />;
} 