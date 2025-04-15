'use client';

import React, { useState, useRef } from 'react';
import { Canvas, useFrame } from '@react-three/fiber';
import { OrbitControls, Sphere, Grid, Text } from '@react-three/drei';
import { Button } from '@/components/ui/button';
import Slider from '@/components/ui/slider';
import { Group } from 'three';

// Helper component for grid points
function GridPoint({ 
  position, 
  size = 0.05, 
  color = "#ff1493" 
}: { 
  position: [number, number, number]; 
  size?: number; 
  color?: string;
}) {
  return (
    <Sphere position={position} args={[size, 4, 4]}>
      <meshStandardMaterial color={color} />
    </Sphere>
  );
}

// Component for gravity well visualization
function GravityWell({ 
  mass = 10, 
  gridSize = 15, 
  gridDensity = 10,
  theory = 'standard'
}: {
  mass: number;
  gridSize?: number;
  gridDensity?: number;
  theory: 'standard' | 'modified';
}) {
  const groupRef = useRef<Group>(null);
  
  // Calculate grid points based on gravity model
  const gridPoints: [number, number, number][] = [];
  const step = gridSize / gridDensity;
  
  for (let i = -gridDensity/2; i <= gridDensity/2; i++) {
    for (let j = -gridDensity/2; j <= gridDensity/2; j++) {
      const x = i * step;
      const z = j * step;
      const distanceFromCenter = Math.sqrt(x * x + z * z);
      
      // Skip the center point to avoid division by zero
      if (distanceFromCenter < 0.1) continue;
      
      // Schwarzschild radius approximation (simplified)
      const rs = 2 * mass * 0.05;
      
      // Calculate depression based on gravity theory
      let depression;
      if (theory === 'standard') {
        // Standard GR: 1/r depression
        depression = -mass / Math.max(distanceFromCenter, 0.5);
      } else {
        // Modified gravity: slower falloff at larger distances
        const a0 = 0.5; // Characteristic acceleration scale
        const mu = distanceFromCenter / (distanceFromCenter + rs);
        depression = -mass / (Math.max(distanceFromCenter, 0.5) * mu);
      }
      
      // Apply limits to depression
      depression = Math.max(depression, -4);
      
      gridPoints.push([x, depression, z]);
    }
  }
  
  // Rotate slowly
  useFrame(() => {
    if (groupRef.current) {
      groupRef.current.rotation.y += 0.001;
    }
  });
  
  return (
    <group ref={groupRef}>
      {/* Gravity Grid */}
      {gridPoints.map((position, i) => (
        <GridPoint key={i} position={position} size={0.1} />
      ))}
      
      {/* Central mass */}
      <Sphere position={[0, -mass/5, 0]} args={[0.5, 32, 32]}>
        <meshStandardMaterial color="#ff1493" />
      </Sphere>
      
      {/* Reference grid */}
      <Grid 
        position={[0, -5, 0]} 
        args={[gridSize*2, gridSize*2, gridSize*2, gridSize*2]} 
        cellColor="#ffffff20"
        sectionColor="#ffffff40"
        fadeDistance={50}
      />
    </group>
  );
}

export function SpacetimeCurvature() {
  const [mass, setMass] = useState(10);
  const [theory, setTheory] = useState<'standard' | 'modified'>('standard');
  
  return (
    <div className="w-full h-[400px]">
      <div className="relative h-full w-full">
        <Canvas camera={{ position: [0, 10, 20], fov: 50 }}>
          <ambientLight intensity={0.5} />
          <pointLight position={[10, 10, 10]} intensity={1} />
          <GravityWell mass={mass} theory={theory} />
          <OrbitControls enableZoom={true} minDistance={5} maxDistance={30} />
          <Text
            position={[0, 5, 0]}
            color="white"
            fontSize={0.8}
            anchorX="center"
            anchorY="middle"
          >
            {theory === 'standard' ? 'Standard General Relativity' : 'Modified Gravity'}
          </Text>
        </Canvas>
        
        <div className="absolute bottom-4 left-4 right-4 p-4 bg-black/50 rounded-md flex flex-col gap-4">
          <div className="flex items-center gap-2">
            <span className="text-white/80 w-24">Mass:</span>
            <Slider
              className="flex-grow"
              value={[mass]}
              min={1}
              max={20}
              step={1}
              onValueChange={(value: number[]) => setMass(value[0])}
            />
            <span className="text-white/80 ml-2 w-10">{mass}</span>
          </div>
          
          <div className="flex gap-2">
            <Button
              variant={theory === 'standard' ? 'default' : 'outline'}
              className={theory === 'standard' ? 'bg-dark-pink hover:bg-dark-pink/80' : 'text-dark-pink'}
              onClick={() => setTheory('standard')}
              size="sm"
            >
              Standard GR
            </Button>
            <Button
              variant={theory === 'modified' ? 'default' : 'outline'}
              className={theory === 'modified' ? 'bg-dark-pink hover:bg-dark-pink/80' : 'text-dark-pink'}
              onClick={() => setTheory('modified')}
              size="sm"
            >
              Modified Gravity
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}

export default SpacetimeCurvature; 