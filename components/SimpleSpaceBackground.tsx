'use client';

import React from 'react';

export default function SimpleSpaceBackground() {
  return (
    <div className="w-full h-full absolute inset-0 bg-black" 
      style={{
        background: 'radial-gradient(ellipse at center, rgba(0,0,0,0.8) 0%, rgba(0,0,0,1) 100%), url("data:image/svg+xml,%3Csvg xmlns=\'http://www.w3.org/2000/svg\' width=\'300\' height=\'300\'%3E%3Cfilter id=\'a\' x=\'0\' y=\'0\'%3E%3CfeTurbulence type=\'fractalNoise\' baseFrequency=\'.8\' stitchTiles=\'stitch\'/%3E%3CfeColorMatrix values=\'0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0.5 0\'/%3E%3C/filter%3E%3Crect width=\'100%25\' height=\'100%25\' filter=\'url(%23a)\'/%3E%3C/svg%3E")',
        backgroundSize: 'cover'
      }}
    >
      <div className="absolute inset-0" style={{
        backgroundImage: 'radial-gradient(circle at 50% 50%, rgba(255, 0, 127, 0.15) 0%, rgba(0, 0, 0, 0) 70%)'
      }}></div>
    </div>
  );
} 