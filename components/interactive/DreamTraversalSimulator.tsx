'use client';

import React, { useState, useEffect, useRef, useCallback } from 'react';
import { Canvas } from '@react-three/fiber';
import { OrbitControls, Stats } from '@react-three/drei';
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
  consciousScale = 1.0,
  boundingRadius = 5.0
}: { 
  dreamState: DreamState;
  isPlaying: boolean;
  identityScale?: number;
  consciousScale?: number;
  boundingRadius?: number;
}) => {
  const particlesRef = useRef<THREE.Points>(null);
  const entanglementRef = useRef<THREE.LineSegments>(null);
  const boundingBoxRef = useRef<THREE.Mesh>(null);
  const identityDimension = dreamState.identity.length;
  const consciousDimension = dreamState.conscious.length;
  
  // Create points based on the dream state
  const vertices: number[] = [];
  const colors: number[] = [];
  const color = new THREE.Color();
  
  // Create particles based on identity vector - with strict containment
  for (let i = 0; i < identityDimension; i++) {
    const angle = (i / identityDimension) * Math.PI * 2;
    
    // Strict containment - normalize all values to fixed range
    // Ensure radius never exceeds bounding sphere
    const normalizedValue = Math.max(-0.8, Math.min(0.8, dreamState.identity[i]));
    const radius = boundingRadius * (0.3 + Math.abs(normalizedValue) * 0.3);
    
    // Add position with strict limits
    vertices.push(
      Math.cos(angle) * radius,
      Math.sin(angle) * radius,
      // Z-axis is strictly limited
      normalizedValue * boundingRadius * 0.3
    );
    
    // Color based on conscious field - blend between blue and pink
    const consciousValue = Math.max(-0.8, Math.min(0.8, dreamState.conscious[i % consciousDimension]));
    color.setRGB(
      0.6 + consciousValue * 0.2,
      0.2 + consciousValue * 0.3,
      0.6 - consciousValue * 0.2
    );
    colors.push(color.r, color.g, color.b);
  }
  
  // Create entanglement connections - strictly limited
  const entanglementPoints: number[] = [];
  const maxConnections = 30; // Further reduced for stability
  let connectionCount = 0;
  
  for (let i = 0; i < dreamState.entanglement.length && connectionCount < maxConnections; i++) {
    for (let j = i + 1; j < dreamState.entanglement[i].length && connectionCount < maxConnections; j++) {
      const strength = Math.abs(dreamState.entanglement[i][j]);
      if (strength > 0.3) { // Higher threshold to show fewer connections
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
        connectionCount++;
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
  
  // Animate particles with VERY controlled rotation
  useFrame((state, delta) => {
    if (particlesRef.current && isPlaying) {
      // Even slower rotation
      particlesRef.current.rotation.y += delta * 0.025; 
      particlesRef.current.rotation.z = Math.sin(state.clock.elapsedTime * 0.05) * 0.05;
      
      // Reset rotation after full circle to prevent numerical drift
      if (particlesRef.current.rotation.y > Math.PI * 2) {
        particlesRef.current.rotation.y -= Math.PI * 2;
      }
    }
    
    if (entanglementRef.current && isPlaying) {
      entanglementRef.current.rotation.y = particlesRef.current?.rotation.y || 0;
      entanglementRef.current.rotation.z = particlesRef.current?.rotation.z || 0;
    }
  });
  
  return (
    <>
      <points ref={particlesRef}>
        <bufferGeometry />
        <pointsMaterial 
          size={0.3} 
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
          opacity={0.15} 
          transparent
          linewidth={1}
        />
      </lineSegments>
      
      {/* Visible bounding sphere to show limits */}
      <mesh ref={boundingBoxRef} visible={true}>
        <sphereGeometry args={[boundingRadius, 16, 16]} />
        <meshBasicMaterial color="#ffffff" wireframe opacity={0.05} transparent />
      </mesh>
    </>
  );
};

// Function to simulate dream step with strong containment
const simulateDreamStep = (
  current: DreamState, 
  instability: number = 0.05, 
  lucidity: number = 0.2
) => {
  // Reduced instability for matrices
  const dEntanglement = randomMatrix(
    current.entanglement.length, 
    current.entanglement[0].length, 
    instability * 0.25 // Greatly reduced for stability
  );
  
  // Even smaller changes for identity
  const dIdentity = randomVector(current.identity.length, instability * 0.3);
  
  // Smaller changes for conscious field
  const dConscious = randomVector(current.conscious.length, instability * 0.5);
  
  // Apply lucidity factor (reduces rate of identity change)
  const lucidityFactor = 1.0 - lucidity;
  
  // Pull-to-center force (prevents values from drifting too far)
  const centeringForce = 0.01;
  
  // Calculate new state with containment forces
  const newState = {
    entanglement: current.entanglement.map((row, i) => 
      row.map((val, j) => {
        // Apply random change but keep within range
        let newVal = val + dEntanglement[i][j];
        // Add centering force - pull values back toward 0
        newVal = newVal - (newVal * centeringForce);
        // Hard clamp values
        return Math.max(-0.8, Math.min(0.8, newVal));
      })
    ),
    identity: current.identity.map((val, i) => {
      // Apply random change with lucidity factor
      let newVal = val + dIdentity[i] * lucidityFactor;
      // Add centering force - pull values back toward 0
      newVal = newVal - (newVal * centeringForce);
      // Hard clamp values
      return Math.max(-0.8, Math.min(0.8, newVal));
    }),
    conscious: current.conscious.map((val, i) => {
      // Apply random change
      let newVal = val + dConscious[i];
      // Add centering force - pull values back toward 0
      newVal = newVal - (newVal * centeringForce);
      // Hard clamp values
      return Math.max(-0.8, Math.min(0.8, newVal));
    }),
    step: current.step + 1,
    time: current.time + 0.1
  };
  
  return newState;
};

// Create initial dream state with moderate values
const createInitialDreamState = (): DreamState => ({
  entanglement: [
    [0.3, 0.1, 0.1, -0.2, 0.1, 0.2],
    [0.1, 0.3, 0.2, 0.1, -0.1, 0.0],
    [0.1, 0.2, 0.3, 0.1, 0.1, -0.1],
    [-0.2, 0.1, 0.1, 0.3, 0.2, 0.1],
    [0.1, -0.1, 0.1, 0.2, 0.3, 0.2],
    [0.2, 0.0, -0.1, 0.1, 0.2, 0.3]
  ],
  identity: [0.3, 0.2, -0.1, 0.2, -0.3, 0.1], // Further reduced initial values
  conscious: [0.3, 0.5, 0.1, -0.2, 0.3, -0.1], // Further reduced initial values
  step: 0,
  time: 0
});

// Main component
export default function DreamTraversalSimulator() {
  const [dreamState, setDreamState] = useState<DreamState>(createInitialDreamState());
  const [isPlaying, setIsPlaying] = useState(false);
  const [instability, setInstability] = useState(0.02); // Even lower default instability
  const [lucidity, setLucidity] = useState(0.4); // Higher default lucidity
  const [autoEntanglement, setAutoEntanglement] = useState(true);
  const [isLucidDream, setIsLucidDream] = useState(false);
  const [showDebug, setShowDebug] = useState(false);
  const intervalRef = useRef<NodeJS.Timeout | null>(null);
  
  // More frequent auto-reset
  useEffect(() => {
    // Automatically reset if things get too unstable
    if (dreamState.step > 0 && dreamState.step % 200 === 0) {
      // More frequent reset to prevent excessive growth
      resetDream();
    }
  }, [dreamState.step]);
  
  // Use memoized step function to prevent recreation on each render
  const stepDream = useCallback(() => {
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
  }, [instability, lucidity]);
  
  // Animation loop for dream traversal
  useEffect(() => {
    if (!isPlaying) {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
        intervalRef.current = null;
      }
      return;
    }
    
    intervalRef.current = setInterval(stepDream, 200); // Lower frame rate for stability
    
    return () => {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
        intervalRef.current = null;
      }
    };
  }, [isPlaying, stepDream]);
  
  const resetDream = () => {
    setDreamState(createInitialDreamState());
    setIsLucidDream(false);
  };
  
  const toggleDream = () => {
    setIsPlaying(!isPlaying);
  };
  
  // Format vector for display
  const formatVector = (vector: number[]) => {
    return vector.map(val => val.toFixed(2)).join(', ');
  };
  
  return (
    <section className="max-h-screen overflow-hidden">
      <h2 className="text-3xl font-bold text-white mb-4">Dream Traversal Simulator</h2>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4 max-h-[calc(100vh-180px)]">
        {/* Dream visualization with container message */}
        <div className="lg:col-span-2 bg-black/30 p-4 rounded-lg border border-dark-pink/20 h-[450px] max-h-[450px] relative overflow-hidden">
          <Canvas 
            shadows 
            camera={{ position: [0, 0, 10], fov: 45 }} // Narrower FOV
            gl={{ antialias: true }}
            dpr={[1, 1.5]} // Even more performance optimized
          >
            <ambientLight intensity={0.4} />
            <pointLight position={[10, 10, 10]} color="#ff2a6d" intensity={0.8} />
            <pointLight position={[-10, -10, -10]} color="#05d9e8" intensity={0.4} />
            
            <DreamParticleField 
              dreamState={dreamState} 
              isPlaying={isPlaying} 
              identityScale={1.0} // Reduced scale
              consciousScale={0.8} // Reduced scale
              boundingRadius={4.0} // Smaller bounding radius
            />
            
            <OrbitControls 
              enablePan={true} 
              enableZoom={true} 
              enableRotate={true}
              maxDistance={12}
              minDistance={5}
              // Limit rotation to prevent disorientation
              maxPolarAngle={Math.PI * 0.6}
              minPolarAngle={Math.PI * 0.3}
            />
            
            {showDebug && <Stats />}
          </Canvas>
          
          <div className="absolute top-4 left-4 text-xs text-white/60 bg-black/30 p-2 rounded">
            Bounded Simulation: All particles contained within sphere
          </div>
          
          {isLucidDream && (
            <div className="absolute top-6 right-6 px-4 py-2 bg-dark-pink/20 rounded-md border border-dark-pink text-white">
              Lucid Dream Detected
            </div>
          )}
          
          <div className="mt-2 flex justify-between items-center">
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
            
            <div className="flex items-center gap-4">
              <div className="text-white/70 text-sm">
                Step: {dreamState.step} | Time: {dreamState.time.toFixed(1)}s
              </div>
              
              <Switch 
                id="show-debug" 
                checked={showDebug}
                onCheckedChange={setShowDebug}
                className="ml-2"
              />
              <Label htmlFor="show-debug" className="text-xs">Debug</Label>
            </div>
          </div>
        </div>
        
        {/* Controls and parameters */}
        <div className="bg-black/30 p-4 rounded-lg border border-dark-pink/20 h-[450px] max-h-[450px] overflow-y-auto">
          <h3 className="text-xl font-semibold text-white mb-4">Dream Parameters</h3>
          
          <div className="space-y-4">
            <div className="space-y-2">
              <div className="flex justify-between items-center">
                <Label>Dream Instability</Label>
                <span className="text-white/70 text-sm">{instability.toFixed(2)}</span>
              </div>
              <Slider 
                value={[instability]} 
                min={0.005} 
                max={0.05} // Much lower max value for stability
                step={0.005} 
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
          
          <div className="mt-4">
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
              </TabsContent>
            </Tabs>
          </div>
        </div>
      </div>
      
      <div className="mt-4 bg-black/30 p-4 rounded-lg border border-dark-pink/20 max-h-[120px] overflow-hidden">
        <div className="flex justify-between items-center mb-2">
          <h3 className="text-xl font-semibold text-white">Current Dream State</h3>
          {isLucidDream ? (
            <span className="text-dark-pink px-3 py-1 bg-dark-pink/10 rounded border border-dark-pink/40">
              Lucid Dreaming
            </span>
          ) : null}
        </div>
        
        <div className="text-white/70 text-sm">
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