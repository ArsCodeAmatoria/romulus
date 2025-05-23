'use client';

import React, { useState, useEffect, useRef } from 'react';
import { Canvas } from '@react-three/fiber';
import { OrbitControls } from '@react-three/drei';
import * as THREE from 'three';
import { motion } from 'framer-motion';
import { Button } from '@/components/ui/button';
import { Slider } from '@/components/ui/slider';
import { Label } from '@/components/ui/label';
import { Card } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Switch } from '@/components/ui/switch';

// Math utility functions
const randomGaussian = () => {
  let u = 0, v = 0;
  while (u === 0) u = Math.random();
  while (v === 0) v = Math.random();
  return Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
};

const randomVector = (size: number, scale = 0.1) => {
  return Array.from({ length: size }, () => randomGaussian() * scale);
};

const randomMatrix = (rows: number, cols: number, scale = 0.1) => {
  return Array.from({ length: rows }, () => randomVector(cols, scale));
};

const addVector = (v1: number[], v2: number[]): number[] => {
  return v1.map((val, i) => val + v2[i]);
};

// Types for dream state
type EntanglementMatrix = number[][];
type IdentityVector = number[];
type ConsciousField = number[];

type DreamState = {
  entanglement: EntanglementMatrix;
  identity: IdentityVector;
  conscious: ConsciousField;
  step: number;
  time: number;
};

// Define custom useFrame hook since we're not using react-three-fiber directly
const useFrame = (callback: (state: any, delta: number) => void) => {
  const requestRef = useRef<number>();
  const previousTimeRef = useRef<number>();
  
  useEffect(() => {
    const animate = (time: number) => {
      if (previousTimeRef.current !== undefined) {
        const deltaTime = (time - previousTimeRef.current) / 1000;
        callback({ clock: { elapsedTime: time / 1000 } }, deltaTime);
      }
      previousTimeRef.current = time;
      requestRef.current = requestAnimationFrame(animate);
    };
    
    requestRef.current = requestAnimationFrame(animate);
    return () => {
      if (requestRef.current) {
        cancelAnimationFrame(requestRef.current);
      }
    };
  }, [callback]);
};

// Visualization components
const DreamParticleField = ({ 
  dreamState, 
  isPlaying,
  identityScale = 1.0,
  consciousScale = 1.0
}: { 
  dreamState: DreamState;
  isPlaying: boolean;
  identityScale?: number;
  consciousScale?: number;
}) => {
  const particlesRef = useRef<THREE.Points>(null);
  const entanglementRef = useRef<THREE.LineSegments>(null);
  const identityDimension = dreamState.identity.length;
  const consciousDimension = dreamState.conscious.length;
  
  // Create points based on the dream state
  const vertices: number[] = [];
  const colors: number[] = [];
  const color = new THREE.Color();
  
  // Create particles based on identity vector
  for (let i = 0; i < identityDimension; i++) {
    const angle = (i / identityDimension) * Math.PI * 2;
    const radius = 3.0 * (0.5 + dreamState.identity[i] * identityScale);
    
    vertices.push(
      Math.cos(angle) * radius,
      Math.sin(angle) * radius,
      dreamState.identity[i] * 2 * identityScale
    );
    
    // Color based on conscious field - blend between blue and pink
    color.setRGB(
      0.8 + dreamState.conscious[i % consciousDimension] * 0.2,
      0.2 + dreamState.conscious[i % consciousDimension] * 0.3,
      0.8 - dreamState.conscious[i % consciousDimension] * 0.2
    );
    colors.push(color.r, color.g, color.b);
  }
  
  // Create entanglement connections
  const entanglementPoints: number[] = [];
  for (let i = 0; i < dreamState.entanglement.length; i++) {
    for (let j = i + 1; j < dreamState.entanglement[i].length; j++) {
      const strength = Math.abs(dreamState.entanglement[i][j]);
      if (strength > 0.1) {
        // Get corresponding vertices for both points
        const idxI = i % (vertices.length / 3);
        const idxJ = j % (vertices.length / 3);
        
        const xi = vertices[idxI * 3];
        const yi = vertices[idxI * 3 + 1];
        const zi = vertices[idxI * 3 + 2];
        
        const xj = vertices[idxJ * 3];
        const yj = vertices[idxJ * 3 + 1];
        const zj = vertices[idxJ * 3 + 2];
        
        entanglementPoints.push(xi, yi, zi);
        entanglementPoints.push(xj, yj, zj);
      }
    }
  }
  
  const particleGeometry = useRef(new THREE.BufferGeometry());
  const entanglementGeometry = useRef(new THREE.BufferGeometry());
  
  useEffect(() => {
    if (particlesRef.current) {
      particleGeometry.current.setAttribute(
        'position', 
        new THREE.Float32BufferAttribute(vertices, 3)
      );
      particleGeometry.current.setAttribute(
        'color', 
        new THREE.Float32BufferAttribute(colors, 3)
      );
      particlesRef.current.geometry = particleGeometry.current;
    }
    
    if (entanglementRef.current) {
      entanglementGeometry.current.setAttribute(
        'position',
        new THREE.Float32BufferAttribute(entanglementPoints, 3)
      );
      entanglementRef.current.geometry = entanglementGeometry.current;
    }
  }, [dreamState, vertices, colors, entanglementPoints]);
  
  // Animate particles
  useFrame((state, delta) => {
    if (particlesRef.current && isPlaying) {
      particlesRef.current.rotation.y += delta * 0.1;
      particlesRef.current.rotation.z = Math.sin(state.clock.elapsedTime * 0.2) * 0.2;
    }
    
    if (entanglementRef.current && isPlaying) {
      entanglementRef.current.rotation.y += delta * 0.1;
      entanglementRef.current.rotation.z = Math.sin(state.clock.elapsedTime * 0.2) * 0.2;
    }
  });
  
  return (
    <>
      <points ref={particlesRef}>
        <bufferGeometry />
        <pointsMaterial 
          size={0.5} 
          sizeAttenuation={true} 
          vertexColors={true}
          transparent
          opacity={0.8}
          depthWrite={false}
        />
      </points>
      
      <lineSegments ref={entanglementRef}>
        <bufferGeometry />
        <lineBasicMaterial 
          color="#ff00ff" 
          opacity={0.3} 
          transparent
          linewidth={1}
        />
      </lineSegments>
    </>
  );
};

// Function to simulate dream step
const simulateDreamStep = (
  current: DreamState, 
  instability: number = 0.05, 
  lucidity: number = 0.2
) => {
  // Simulate entanglement matrix changes
  const dEntanglement = randomMatrix(
    current.entanglement.length, 
    current.entanglement[0].length, 
    instability
  );
  
  // Simulate identity vector changes
  const dIdentity = randomVector(current.identity.length, instability);
  
  // Simulate conscious field changes
  const dConscious = randomVector(current.conscious.length, instability);
  
  // Apply lucidity factor (reduces rate of identity change)
  const lucidityFactor = 1.0 - lucidity;
  
  return {
    entanglement: current.entanglement.map((row, i) => 
      row.map((val, j) => {
        // Apply random change but keep within range
        const newVal = val + dEntanglement[i][j];
        return Math.max(-1, Math.min(1, newVal));
      })
    ),
    identity: current.identity.map((val, i) => {
      // Apply random change with lucidity factor but keep within range
      const newVal = val + dIdentity[i] * lucidityFactor;
      return Math.max(-1, Math.min(1, newVal));
    }),
    conscious: current.conscious.map((val, i) => {
      // Apply random change but keep within range
      const newVal = val + dConscious[i];
      return Math.max(-1, Math.min(1, newVal));
    }),
    step: current.step + 1,
    time: current.time + 0.1
  };
};

// Create initial dream state
const createInitialDreamState = (): DreamState => ({
  entanglement: [
    [0.5, 0.2, 0.1, -0.3, 0.1, 0.4],
    [0.2, 0.6, 0.3, 0.1, -0.2, 0.0],
    [0.1, 0.3, 0.7, 0.2, 0.1, -0.1],
    [-0.3, 0.1, 0.2, 0.6, 0.4, 0.2],
    [0.1, -0.2, 0.1, 0.4, 0.5, 0.3],
    [0.4, 0.0, -0.1, 0.2, 0.3, 0.4]
  ],
  identity: [0.7, 0.3, -0.2, 0.4, -0.6, 0.1],
  conscious: [0.5, 0.8, 0.2, -0.3, 0.4, -0.1],
  step: 0,
  time: 0
});

// Main component
export default function DreamTraversalSimulator() {
  const [dreamState, setDreamState] = useState<DreamState>(createInitialDreamState());
  const [isPlaying, setIsPlaying] = useState(false);
  const [instability, setInstability] = useState(0.05);
  const [lucidity, setLucidity] = useState(0.2);
  const [autoEntanglement, setAutoEntanglement] = useState(true);
  const [isLucidDream, setIsLucidDream] = useState(false);
  
  // Animation loop for dream traversal
  useEffect(() => {
    if (!isPlaying) return;
    
    const interval = setInterval(() => {
      setDreamState(current => {
        // Calculate new dream state
        const newState = simulateDreamStep(current, instability, lucidity);
        
        // Check for lucid dreaming (low rate of change in identity)
        const identityChangeRate = Math.abs(
          newState.identity.reduce((sum, val, i) => sum + Math.abs(val - current.identity[i]), 0) / 
          current.identity.length
        );
        
        if (identityChangeRate < 0.01 && lucidity > 0.5) {
          setIsLucidDream(true);
        } else {
          setIsLucidDream(false);
        }
        
        return newState;
      });
    }, 150);
    
    return () => clearInterval(interval);
  }, [isPlaying, instability, lucidity]);
  
  const resetDream = () => {
    setDreamState(createInitialDreamState());
    setIsLucidDream(false);
  };
  
  const toggleDream = () => {
    setIsPlaying(!isPlaying);
  };
  
  // Format matrix for display
  const formatMatrix = (matrix: number[][]) => {
    const formatted = matrix.map(row => 
      row.map(val => val.toFixed(2)).join(', ')
    ).join('\n');
    return formatted;
  };
  
  // Format vector for display
  const formatVector = (vector: number[]) => {
    return vector.map(val => val.toFixed(2)).join(', ');
  };
  
  return (
    <section>
      <h2 className="text-3xl font-bold text-white mb-6">Dream Traversal Simulator</h2>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Dream visualization */}
        <div className="lg:col-span-2 bg-black/30 p-6 rounded-lg border border-dark-pink/20 min-h-[500px] relative">
          <Canvas shadows camera={{ position: [0, 0, 10], fov: 50 }}>
            <ambientLight intensity={0.5} />
            <pointLight position={[10, 10, 10]} color="#ff2a6d" intensity={1} />
            <pointLight position={[-10, -10, -10]} color="#05d9e8" intensity={0.5} />
            
            <DreamParticleField 
              dreamState={dreamState} 
              isPlaying={isPlaying} 
              identityScale={2.0}
              consciousScale={1.5}
            />
            
            <OrbitControls enablePan={true} enableZoom={true} enableRotate={true} />
          </Canvas>
          
          {isLucidDream && (
            <div className="absolute top-6 right-6 px-4 py-2 bg-dark-pink/20 rounded-md border border-dark-pink text-white">
              Lucid Dream Detected
            </div>
          )}
          
          <div className="mt-4 flex justify-between items-center">
            <div className="flex gap-3">
              <Button 
                onClick={toggleDream}
                variant="outline"
                className="border-dark-pink text-white hover:bg-dark-pink/20"
              >
                {isPlaying ? 'Pause Dream' : 'Start Dream'}
              </Button>
              
              <Button 
                onClick={resetDream}
                variant="outline"
                className="border-dark-pink text-white hover:bg-dark-pink/20"
              >
                Reset
              </Button>
            </div>
            
            <div className="text-white/70">
              Step: {dreamState.step} | Time: {dreamState.time.toFixed(1)}s
            </div>
          </div>
        </div>
        
        {/* Controls and parameters */}
        <div className="bg-black/30 p-6 rounded-lg border border-dark-pink/20">
          <h3 className="text-xl font-semibold text-white mb-4">Dream Parameters</h3>
          
          <div className="space-y-6">
            <div className="space-y-2">
              <div className="flex justify-between items-center">
                <Label>Dream Instability</Label>
                <span className="text-white/70 text-sm">{instability.toFixed(2)}</span>
              </div>
              <Slider 
                value={[instability]} 
                min={0.01} 
                max={0.2} 
                step={0.01} 
                onValueChange={values => setInstability(values[0])}
              />
              <p className="text-white/50 text-xs mt-1">Controls rate of change in dream state</p>
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between items-center">
                <Label>Lucidity</Label>
                <span className="text-white/70 text-sm">{lucidity.toFixed(2)}</span>
              </div>
              <Slider 
                value={[lucidity]} 
                min={0.0} 
                max={1.0} 
                step={0.05} 
                onValueChange={values => setLucidity(values[0])}
              />
              <p className="text-white/50 text-xs mt-1">Higher values stabilize identity</p>
            </div>
            
            <div className="flex items-center space-x-2 pt-2">
              <Switch 
                id="auto-entanglement" 
                checked={autoEntanglement}
                onCheckedChange={setAutoEntanglement}
              />
              <Label htmlFor="auto-entanglement">Automatic Entanglement</Label>
            </div>
          </div>
          
          <div className="mt-6">
            <Tabs defaultValue="identity">
              <TabsList className="w-full">
                <TabsTrigger value="identity" className="flex-1">Identity (7D)</TabsTrigger>
                <TabsTrigger value="conscious" className="flex-1">Conscious (8D)</TabsTrigger>
              </TabsList>
              
              <TabsContent value="identity" className="mt-4">
                <Card className="bg-black/50 p-4 font-mono text-xs">
                  <pre className="text-white/80 whitespace-pre-wrap">
                    {formatVector(dreamState.identity)}
                  </pre>
                </Card>
                <p className="text-white/50 text-xs mt-2">
                  Identity vector determines "who" you are in the dream
                </p>
              </TabsContent>
              
              <TabsContent value="conscious" className="mt-4">
                <Card className="bg-black/50 p-4 font-mono text-xs">
                  <pre className="text-white/80 whitespace-pre-wrap">
                    {formatVector(dreamState.conscious)}
                  </pre>
                </Card>
                <p className="text-white/50 text-xs mt-2">
                  Conscious field determines "how" you experience the dream
                </p>
              </TabsContent>
            </Tabs>
          </div>
        </div>
      </div>
      
      <div className="mt-6 bg-black/30 p-6 rounded-lg border border-dark-pink/20">
        <h3 className="text-xl font-semibold text-white mb-4">Current Dream State</h3>
        
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Card className="bg-black/50 p-4">
            <h4 className="text-white text-sm font-medium mb-2">6D Entanglement</h4>
            <div className="text-white/70 text-xs font-mono h-24 overflow-y-auto">
              <pre>{formatMatrix(dreamState.entanglement)}</pre>
            </div>
          </Card>
          
          <Card className="bg-black/50 p-4">
            <h4 className="text-white text-sm font-medium mb-2">7D Identity Vector</h4>
            <div className="text-white/70 text-xs font-mono">
              <pre>{formatVector(dreamState.identity)}</pre>
            </div>
            <div className="mt-2 space-x-1">
              {dreamState.identity.map((val, idx) => (
                <span 
                  key={idx} 
                  className="inline-block w-3 h-3 rounded-full" 
                  style={{ 
                    backgroundColor: `rgba(255, ${Math.floor(128 + val * 127)}, ${Math.floor(200 + val * 55)}, 0.8)`,
                    transform: `scale(${0.8 + Math.abs(val) * 0.5})`
                  }}
                />
              ))}
            </div>
          </Card>
          
          <Card className="bg-black/50 p-4">
            <h4 className="text-white text-sm font-medium mb-2">8D Conscious Field</h4>
            <div className="text-white/70 text-xs font-mono">
              <pre>{formatVector(dreamState.conscious)}</pre>
            </div>
            <div className="mt-2 space-x-1">
              {dreamState.conscious.map((val, idx) => (
                <span 
                  key={idx} 
                  className="inline-block w-3 h-3 rounded-full" 
                  style={{ 
                    backgroundColor: `rgba(${Math.floor(128 + val * 127)}, ${Math.floor(50 + val * 50)}, 255, 0.8)`,
                    transform: `scale(${0.8 + Math.abs(val) * 0.5})`
                  }}
                />
              ))}
            </div>
          </Card>
        </div>
        
        <div className="mt-4 text-white/70 text-sm">
          {isLucidDream ? (
            <p className="text-dark-pink">
              Lucid Dream State: You are aware that you are dreaming and can control the dream.
            </p>
          ) : (
            <p>
              Standard Dream State: Dream unfolds naturally with changes across all dimensions.
            </p>
          )}
        </div>
      </div>
    </section>
  );
} 