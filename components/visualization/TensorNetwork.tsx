'use client';

import React, { useRef } from 'react';
import { Canvas, useFrame } from '@react-three/fiber';
import { OrbitControls, Sphere, Line, Text } from '@react-three/drei';
import * as THREE from 'three';
import { Group } from 'three';

// Function to generate positions in a hyperbolic disc pattern
function generateHyperbolicPositions(numNodes: number, scale: number = 5): [number, number, number][] {
  const positions: [number, number, number][] = [];
  
  // Center node
  positions.push([0, 0, 0]);
  
  // Place nodes in concentric circles with increasing density
  let ringIndex = 1;
  let remainingNodes = numNodes - 1;
  
  while (remainingNodes > 0) {
    // Number of nodes in this ring
    const nodesInRing = Math.min(remainingNodes, 3 * ringIndex);
    
    // Radius for this ring (using hyperbolic scaling)
    const radius = Math.tanh(ringIndex * 0.5) * scale;
    
    // Place nodes evenly around the ring
    for (let i = 0; i < nodesInRing; i++) {
      const angle = (i / nodesInRing) * Math.PI * 2;
      const x = radius * Math.cos(angle);
      const z = radius * Math.sin(angle);
      // Add some vertical variation
      const y = (Math.random() * 0.5 - 0.25) * (ringIndex * 0.3);
      positions.push([x, y, z]);
    }
    
    remainingNodes -= nodesInRing;
    ringIndex++;
  }
  
  return positions;
}

// Function to generate edges between nodes
function generateEdges(positions: [number, number, number][], density: number = 0.7): number[][] {
  const edges: number[][] = [];
  
  // Connect center to first ring
  for (let i = 1; i < positions.length; i++) {
    if (distance(positions[0], positions[i]) < 3) {
      edges.push([0, i]);
    }
  }
  
  // Connect other nodes based on distance
  for (let i = 1; i < positions.length; i++) {
    for (let j = i + 1; j < positions.length; j++) {
      // Calculate distance between nodes
      const dist = distance(positions[i], positions[j]);
      
      // Add edge with probability based on distance and density
      const probability = density * Math.exp(-dist / 2);
      if (Math.random() < probability && dist < 3) {
        edges.push([i, j]);
      }
    }
  }
  
  return edges;
}

// Helper to calculate distance between two 3D points
function distance(p1: [number, number, number], p2: [number, number, number]): number {
  return Math.sqrt(
    Math.pow(p1[0] - p2[0], 2) + 
    Math.pow(p1[1] - p2[1], 2) + 
    Math.pow(p1[2] - p2[2], 2)
  );
}

// Component to render nodes and edges
function TensorNetworkGraph() {
  const groupRef = useRef<Group>(null);
  
  // Generate data
  const numNodes = 25;
  const positions = generateHyperbolicPositions(numNodes);
  const edges = generateEdges(positions);
  
  // Colors based on position
  const getNodeColor = (index: number, position: [number, number, number]) => {
    if (index === 0) return "#ff1493"; // Center node (dark pink)
    const dist = distance([0, 0, 0], position);
    return `hsl(320, 100%, ${Math.max(40, 70 - dist * 10)}%)`;
  };
  
  // Animation
  useFrame((_, delta) => {
    if (groupRef.current) {
      groupRef.current.rotation.y += delta * 0.2;
      groupRef.current.rotation.x = Math.sin(Date.now() * 0.0005) * 0.2;
    }
  });

  return (
    <group ref={groupRef}>
      {/* Render edges */}
      {edges.map((edge, idx) => {
        const start = positions[edge[0]];
        const end = positions[edge[1]];
        
        // Calculate edge intensity based on distance from center
        const distFromCenter = (distance([0, 0, 0], start) + distance([0, 0, 0], end)) / 2;
        const opacity = Math.max(0.2, 1 - distFromCenter * 0.2);
        
        return (
          <Line
            key={`edge-${idx}`}
            points={[new THREE.Vector3(...start), new THREE.Vector3(...end)]}
            color="#ff1493"
            lineWidth={2}
            opacity={opacity}
            transparent={true}
          />
        );
      })}
      
      {/* Render nodes */}
      {positions.map((position, idx) => {
        const distFromCenter = distance([0, 0, 0], position);
        const size = idx === 0 ? 0.25 : Math.max(0.08, 0.18 - distFromCenter * 0.02);
        
        return (
          <Sphere key={`node-${idx}`} position={position} args={[size, 10, 10]}>
            <meshStandardMaterial 
              color={getNodeColor(idx, position)} 
              emissive={getNodeColor(idx, position)}
              emissiveIntensity={0.5}
            />
          </Sphere>
        );
      })}
    </group>
  );
}

export default function TensorNetwork() {
  return (
    <div className="w-full h-[400px] bg-black/20 rounded-md overflow-hidden">
      <Canvas camera={{ position: [0, 3, 8], fov: 50 }}>
        <ambientLight intensity={0.3} />
        <pointLight position={[0, 5, 5]} intensity={0.8} color="#ffffff" />
        <TensorNetworkGraph />
        <OrbitControls
          enableZoom={true}
          enablePan={false}
          minDistance={4}
          maxDistance={16}
          autoRotate
          autoRotateSpeed={0.5}
        />
        <Text
          position={[0, -2.8, 0]}
          color="white"
          fontSize={0.25}
          anchorX="center"
          anchorY="middle"
          fillOpacity={0.7}
        >
          Hyperbolic Tensor Network Structure
        </Text>
      </Canvas>
    </div>
  );
} 