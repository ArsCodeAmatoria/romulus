'use client';

import { useRef, useState, useEffect } from 'react';
import { Canvas, useFrame } from '@react-three/fiber';
import { OrbitControls, Line, Sphere, Text } from '@react-three/drei';
import * as THREE from 'three';

// Type definitions
interface TensorNode {
  position: [number, number, number];
  connections: number[];
  id: number;
}

interface TensorConnection {
  from: number;
  to: number;
}

interface NetworkData {
  nodes: TensorNode[];
  connections: TensorConnection[];
}

interface TensorNodeProps {
  position: [number, number, number];
  size?: number;
  highlight?: boolean;
}

interface TensorConnectionsProps {
  nodes: TensorNode[];
  connections: TensorConnection[];
  highlightMode: 'none' | 'boundary' | 'bulk';
}

interface SceneProps {
  highlightMode: 'none' | 'boundary' | 'bulk';
}

interface InfoLabelsProps {
  highlightMode: 'none' | 'boundary' | 'bulk';
}

// Generate a tensor network with nodes and connections
function generateTensorNetwork(numNodes = 32, radius = 6, connectionDensity = 0.6): NetworkData {
  // Position nodes in a hyperbolic pattern
  const nodes: TensorNode[] = [];
  const phi = (Math.sqrt(5) + 1) / 2; // Golden ratio for nice distribution
  
  for (let i = 0; i < numNodes; i++) {
    // Use fibonacci sphere distribution for node placement
    const y = 1 - (i / (numNodes - 1)) * 2; // y goes from 1 to -1
    const radiusAtY = Math.sqrt(1 - y * y) * radius; // Radius at this y level
    
    // Apply some curvature to create hyperbolic-like space
    const hyperbolicFactor = 1.5;
    const curvedY = y * (radius / hyperbolicFactor);
    
    const theta = 2 * Math.PI * i * phi; // Golden angle
    const x = Math.cos(theta) * radiusAtY;
    const z = Math.sin(theta) * radiusAtY;
    
    nodes.push({ 
      position: [x, curvedY, z], 
      connections: [],
      id: i
    });
  }
  
  // Create connections between nodes (tensor contractions)
  const connections: TensorConnection[] = [];
  for (let i = 0; i < numNodes; i++) {
    // Each node is connected to several others
    const numConnections = 2 + Math.floor(Math.random() * 3); // 2-4 connections per node
    
    for (let j = 0; j < numConnections; j++) {
      if (Math.random() > connectionDensity) continue;
      
      // Find a node to connect to - prefer nodes that are closer in space
      const possibleTargets: Array<{id: number, distance: number}> = [];
      for (let k = 0; k < numNodes; k++) {
        if (k !== i && !nodes[i].connections.includes(k)) {
          const distance = Math.sqrt(
            Math.pow(nodes[i].position[0] - nodes[k].position[0], 2) +
            Math.pow(nodes[i].position[1] - nodes[k].position[1], 2) +
            Math.pow(nodes[i].position[2] - nodes[k].position[2], 2)
          );
          possibleTargets.push({ id: k, distance });
        }
      }
      
      // Sort by distance and pick one with preference to closer nodes
      if (possibleTargets.length > 0) {
        possibleTargets.sort((a, b) => a.distance - b.distance);
        const targetIndex = Math.min(
          Math.floor(Math.pow(Math.random(), 2) * possibleTargets.length),
          possibleTargets.length - 1
        );
        const targetId = possibleTargets[targetIndex].id;
        
        // Add connection if it doesn't already exist
        if (!connections.some(c => 
          (c.from === i && c.to === targetId) || 
          (c.from === targetId && c.to === i)
        )) {
          connections.push({ from: i, to: targetId });
          nodes[i].connections.push(targetId);
          nodes[targetId].connections.push(i);
        }
      }
    }
  }
  
  return { nodes, connections };
}

// Component for a tensor network node
function TensorNode({ position, size = 0.15, highlight = false }: TensorNodeProps) {
  return (
    <Sphere position={position} args={[size, 16, 16]}>
      <meshStandardMaterial 
        color={highlight ? "#ffffff" : "#ff007f"} 
        emissive={highlight ? "#ffffff" : "#ff007f"}
        emissiveIntensity={highlight ? 0.8 : 0.5}
      />
    </Sphere>
  );
}

// Component for tensor connections (edges)
function TensorConnections({ nodes, connections, highlightMode }: TensorConnectionsProps) {
  const lines = connections.map((connection, index) => {
    const start = new THREE.Vector3(...nodes[connection.from].position);
    const end = new THREE.Vector3(...nodes[connection.to].position);
    
    // Determine if this connection should be highlighted
    const isHighlighted = 
      highlightMode === 'boundary' 
        ? isBoundaryConnection(nodes, connection) 
        : highlightMode === 'bulk' 
          ? !isBoundaryConnection(nodes, connection) 
          : false;
    
    return (
      <Line 
        key={index}
        points={[start, end]}
        color={isHighlighted ? "#ffffff" : "#ff007f"} 
        lineWidth={isHighlighted ? 2 : 1}
        opacity={isHighlighted ? 1 : 0.7}
        transparent
      />
    );
  });
  
  return <>{lines}</>;
}

// Helper to determine if a connection is part of the boundary
function isBoundaryConnection(nodes: TensorNode[], connection: TensorConnection): boolean {
  // Simple heuristic: connections to nodes near the edge of the network
  // are considered boundary connections
  const fromPos = nodes[connection.from].position;
  const toPos = nodes[connection.to].position;
  
  // Calculate distance from center
  const fromDist = Math.sqrt(fromPos[0]**2 + fromPos[2]**2);
  const toDist = Math.sqrt(toPos[0]**2 + toPos[2]**2);
  
  // If either node is near the boundary
  return fromDist > 4.5 || toDist > 4.5;
}

// Animation controller for tensor network
function TensorNetworkAnimation({ highlightMode }: SceneProps) {
  const networkRef = useRef<NetworkData>(generateTensorNetwork());
  const [nodes, setNodes] = useState<TensorNode[]>(networkRef.current.nodes);
  const [connections, setConnections] = useState<TensorConnection[]>(networkRef.current.connections);
  
  // Animated rotation of the entire network
  useFrame((state, delta) => {
    const time = state.clock.getElapsedTime();
    const rotationSpeed = 0.15;
    
    // Rotate nodes around the y-axis
    const updatedNodes = nodes.map(node => {
      const [x, y, z] = node.position;
      const angle = time * rotationSpeed;
      
      return {
        ...node,
        position: [
          x * Math.cos(angle) - z * Math.sin(angle),
          y,
          x * Math.sin(angle) + z * Math.cos(angle)
        ] as [number, number, number]
      };
    });
    
    setNodes(updatedNodes);
  });
  
  // Render nodes and connections
  return (
    <>
      {nodes.map((node, index) => (
        <TensorNode 
          key={index} 
          position={node.position} 
          highlight={
            highlightMode === 'boundary' ? 
              Math.sqrt(node.position[0]**2 + node.position[2]**2) > 4.5 : 
              highlightMode === 'bulk' ? 
                Math.sqrt(node.position[0]**2 + node.position[2]**2) < 3 : 
                false
          }
        />
      ))}
      <TensorConnections 
        nodes={nodes} 
        connections={connections} 
        highlightMode={highlightMode} 
      />
    </>
  );
}

// Information labels
function InfoLabels({ highlightMode }: InfoLabelsProps) {
  // Different labels based on the highlight mode
  let label = "Tensor Network";
  let description = "Representing quantum entanglement";
  
  if (highlightMode === 'boundary') {
    label = "Boundary Theory";
    description = "2D Conformal Field Theory";
  } else if (highlightMode === 'bulk') {
    label = "Bulk Spacetime";
    description = "3D Anti-de Sitter Space";
  }
  
  return (
    <>
      <Text
        position={[0, 2.5, 0]}
        fontSize={0.5}
        color="#ffffff"
        anchorX="center"
        anchorY="middle"
      >
        {label}
      </Text>
      <Text
        position={[0, 2, 0]}
        fontSize={0.3}
        color="#ff007f"
        anchorX="center"
        anchorY="middle"
      >
        {description}
      </Text>
    </>
  );
}

// Main scene component
function Scene({ highlightMode }: SceneProps) {
  return (
    <>
      <ambientLight intensity={0.2} />
      <pointLight position={[10, 10, 10]} intensity={1} color="#ffffff" />
      <TensorNetworkAnimation highlightMode={highlightMode} />
      <InfoLabels highlightMode={highlightMode} />
      <OrbitControls 
        enablePan={false}
        minDistance={5}
        maxDistance={25}
        autoRotate
        autoRotateSpeed={0.5}
      />
    </>
  );
}

// Main component with mode selection
export default function TensorNetworkVisualization() {
  const [highlightMode, setHighlightMode] = useState<'none' | 'boundary' | 'bulk'>('none');
  
  return (
    <div className="w-full h-60 relative">
      <div className="absolute bottom-4 right-4 z-10 flex space-x-2">
        <button 
          onClick={() => setHighlightMode('none')}
          className={`px-3 py-1 text-xs rounded-full ${highlightMode === 'none' ? 'bg-dark-pink text-white' : 'bg-zinc-800 text-white/70'}`}
        >
          Full Network
        </button>
        <button 
          onClick={() => setHighlightMode('boundary')}
          className={`px-3 py-1 text-xs rounded-full ${highlightMode === 'boundary' ? 'bg-dark-pink text-white' : 'bg-zinc-800 text-white/70'}`}
        >
          Boundary (CFT)
        </button>
        <button 
          onClick={() => setHighlightMode('bulk')}
          className={`px-3 py-1 text-xs rounded-full ${highlightMode === 'bulk' ? 'bg-dark-pink text-white' : 'bg-zinc-800 text-white/70'}`}
        >
          Bulk (AdS)
        </button>
      </div>
      
      <Canvas camera={{ position: [0, 0, 15], fov: 40 }}>
        <Scene highlightMode={highlightMode} />
      </Canvas>
    </div>
  );
} 