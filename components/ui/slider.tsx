"use client";

import * as React from "react";
import { cn } from "@/lib/utils";

interface SliderProps {
  value: number[];
  min: number;
  max: number;
  step?: number;
  className?: string;
  onValueChange: (value: number[]) => void;
}

export function Slider({
  value,
  min,
  max,
  step = 1,
  className,
  onValueChange,
}: SliderProps) {
  const [localValue, setLocalValue] = React.useState<number>(value[0]);
  const range = max - min;

  // Calculate percentage for styling
  const percentage = ((localValue - min) / range) * 100;

  // Handle slider change
  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = parseFloat(e.target.value);
    setLocalValue(newValue);
    onValueChange([newValue]);
  };

  // Update local value when prop value changes
  React.useEffect(() => {
    if (value[0] !== localValue) {
      setLocalValue(value[0]);
    }
  }, [value, localValue]);

  return (
    <div className={cn("relative flex w-full touch-none select-none items-center", className)}>
      <div className="relative h-2 w-full grow overflow-hidden rounded-full bg-zinc-900/50">
        {/* Filled track */}
        <div 
          className="absolute h-full bg-dark-pink"
          style={{ width: `${percentage}%` }}
        />
      </div>
      <input
        type="range"
        min={min}
        max={max}
        step={step}
        value={localValue}
        onChange={handleChange}
        className="absolute inset-0 h-full w-full cursor-pointer opacity-0"
        aria-label="Slider"
      />
      {/* Thumb */}
      <div 
        className="absolute h-5 w-5 rounded-full border-2 border-dark-pink bg-black"
        style={{ 
          left: `${percentage}%`, 
          transform: 'translateX(-50%)',
          pointerEvents: 'none'
        }}
      />
    </div>
  );
}

export default Slider; 