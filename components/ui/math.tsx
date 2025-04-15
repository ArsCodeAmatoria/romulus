'use client';

import { useEffect, useRef } from 'react';
import katex from 'katex';
import 'katex/dist/katex.min.css';

interface MathProps {
  math: string;
  display?: boolean;
  className?: string;
}

export function Math({ math, display = false, className = '' }: MathProps) {
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (containerRef.current) {
      katex.render(math, containerRef.current, {
        displayMode: display,
        throwOnError: false,
      });
    }
  }, [math, display]);

  return <div ref={containerRef} className={className} />;
}

export function InlineMath({ math, className = '' }: Omit<MathProps, 'display'>) {
  return <Math math={math} display={false} className={className} />;
}

export function DisplayMath({ math, className = '' }: Omit<MathProps, 'display'>) {
  return <Math math={math} display={true} className={className} />;
} 