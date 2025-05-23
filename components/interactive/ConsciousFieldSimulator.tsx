'use client';

import React, { useRef, useState, useEffect } from 'react';
import { Canvas, useFrame } from '@react-three/fiber';
import { OrbitControls, Text } from '@react-three/drei';
import * as THREE from 'three';
import { motion } from 'framer-motion';
import { Slider } from '@/components/ui/slider';
import { Button } from '@/components/ui/button';
import { Switch } from '@/components/ui/switch';
import { Label } from '@/components/ui/label';

// Define particle types representing different aspects of consciousness
const PARTICLE_TYPES = {
  LOGICAL: { color: new THREE.Color('#00ffff'), name: 'Logical' },
  EMOTIONAL: { color: new THREE.Color('#ff00ff'), name: 'Emotional' },
  CREATIVE: { color: new THREE.Color('#ffff00'), name: 'Creative' },
  INTUITIVE: { color: new THREE.Color('#00ff00'), name: 'Intuitive' },
  CRITICAL: { color: new THREE.Color('#ff0000'), name: 'Critical' },
};

type ParticleType = {
  color: THREE.Color;
  name: string;
};

type PsiParticleProps = {
  position: [number, number, number];
  velocity: {
    x: number;
    y: number;
    z: number;
  };
  type: ParticleType;
  energy: number;
  coherence: number;
  onClick: () => void;
  selected: boolean;
};

type Particle = {
  id: number;
  position: [number, number, number];
  velocity: {
    x: number;
    y: number;
    z: number;
  };
  type: ParticleType;
  energy: number;
  coherence: number;
};

type ParticleConnectionsProps = {
  particles: Particle[];
  entanglementThreshold: number;
};

type ConsciousFieldSceneProps = {
  particleCount: number;
  waveCoherence: number;
  entanglementStrength: number;
  collapseRate: number;
  quantumFluctuations: number;
};

// A single ψ-particle representing a sub-personality or consciousness fragment
function PsiParticle({ position, velocity, type, energy, coherence, onClick, selected }: PsiParticleProps) {
  const ref = useRef<THREE.Mesh>(null);
  const [hovered, setHovered] = useState(false);
  
  // Update particle position based on velocity and connect to neighboring particles
  useFrame((state, delta) => {
    if (ref.current) {
      ref.current.position.x += velocity.x * delta * energy;
      ref.current.position.y += velocity.y * delta * energy;
      ref.current.position.z += velocity.z * delta * energy;
      
      // Bounce off boundaries
      if (Math.abs(ref.current.position.x) > 5) velocity.x *= -1;
      if (Math.abs(ref.current.position.y) > 5) velocity.y *= -1;
      if (Math.abs(ref.current.position.z) > 5) velocity.z *= -1;
      
      // Pulse based on coherence
      const scale = 0.2 + Math.sin(state.clock.elapsedTime * coherence * 2) * 0.05;
      ref.current.scale.set(scale, scale, scale);
    }
  });

  return (
    <mesh
      ref={ref}
      position={position}
      onClick={onClick}
      onPointerOver={() => setHovered(true)}
      onPointerOut={() => setHovered(false)}
    >
      <sphereGeometry args={[0.2, 32, 32]} />
      <meshStandardMaterial 
        color={type.color} 
        emissive={type.color} 
        emissiveIntensity={hovered || selected ? 2 : 0.5} 
        transparent
        opacity={coherence}
      />
      {(hovered || selected) && (
        <Text
          position={[0, 0.4, 0]}
          fontSize={0.2}
          color="white"
          anchorX="center"
          anchorY="middle"
        >
          {type.name}
        </Text>
      )}
    </mesh>
  );
}

// Connections between particles representing entanglement
function ParticleConnections({ particles, entanglementThreshold }: ParticleConnectionsProps) {
  const connections = [];
  
  for (let i = 0; i < particles.length; i++) {
    for (let j = i + 1; j < particles.length; j++) {
      const distance = new THREE.Vector3()
        .fromArray(particles[i].position)
        .distanceTo(new THREE.Vector3().fromArray(particles[j].position));
        
      if (distance < entanglementThreshold) {
        connections.push({
          start: particles[i].position,
          end: particles[j].position,
          strength: 1 - distance / entanglementThreshold,
          key: `${i}-${j}`
        });
      }
    }
  }
  
  return (
    <>
      {connections.map(conn => {
        // Create vertices array from start and end points
        const vertices = new Float32Array([...conn.start, ...conn.end]);
        
        return (
          <line key={conn.key}>
            <bufferGeometry>
              <bufferAttribute 
                attach="attributes-position"
                args={[vertices, 3]}
              />
            </bufferGeometry>
            <lineBasicMaterial
              color="#ffffff"
              opacity={conn.strength * 0.5}
              transparent
              linewidth={1}
            />
          </line>
        );
      })}
    </>
  );
}

// Main simulation scene
function ConsciousFieldScene({ 
  particleCount, 
  waveCoherence, 
  entanglementStrength, 
  collapseRate,
  quantumFluctuations
}: ConsciousFieldSceneProps) {
  const [particles, setParticles] = useState<Particle[]>([]);
  const [selectedParticle, setSelectedParticle] = useState<number | null>(null);
  
  // Initialize particles
  useEffect(() => {
    const types = Object.values(PARTICLE_TYPES);
    const newParticles = Array.from({ length: particleCount }, (_, i) => ({
      id: i,
      position: [
        (Math.random() - 0.5) * 8,
        (Math.random() - 0.5) * 8,
        (Math.random() - 0.5) * 8,
      ] as [number, number, number],
      velocity: {
        x: (Math.random() - 0.5) * 0.5,
        y: (Math.random() - 0.5) * 0.5,
        z: (Math.random() - 0.5) * 0.5,
      },
      type: types[Math.floor(Math.random() * types.length)],
      energy: 0.5 + Math.random() * 0.5,
      coherence: waveCoherence * (0.7 + Math.random() * 0.3)
    }));
    setParticles(newParticles);
    setSelectedParticle(null);
  }, [particleCount]);
  
  // Quantum fluctuations effect
  useFrame((state, delta) => {
    if (quantumFluctuations > 0 && Math.random() < delta * quantumFluctuations) {
      setParticles(prevParticles => {
        const newParticles = [...prevParticles];
        const particleIndex = Math.floor(Math.random() * newParticles.length);
        const particle = {...newParticles[particleIndex]};
        
        // Random quantum jump
        particle.position = [
          particle.position[0] + (Math.random() - 0.5) * 2,
          particle.position[1] + (Math.random() - 0.5) * 2,
          particle.position[2] + (Math.random() - 0.5) * 2,
        ] as [number, number, number];
        
        newParticles[particleIndex] = particle;
        return newParticles;
      });
    }
  });
  
  // Effect of collapse rate (measurement/observation)
  useFrame((state, delta) => {
    if (collapseRate > 0 && Math.random() < delta * collapseRate) {
      // Wavefunction collapse simulation
      const collapsePoint = [
        (Math.random() - 0.5) * 10,
        (Math.random() - 0.5) * 10,
        (Math.random() - 0.5) * 10,
      ] as [number, number, number];
      
      setParticles(prevParticles => {
        return prevParticles.map(particle => {
          const distance = new THREE.Vector3()
            .fromArray(particle.position)
            .distanceTo(new THREE.Vector3().fromArray(collapsePoint));
          
          if (distance < 3) {
            // Particles near collapse point are affected
            return {
              ...particle,
              velocity: {
                x: particle.velocity.x * 0.5,
                y: particle.velocity.y * 0.5,
                z: particle.velocity.z * 0.5,
              },
              coherence: particle.coherence * 0.9, // Reduce coherence
            };
          }
          return particle;
        });
      });
    }
  });
  
  const handleParticleClick = (id: number) => {
    setSelectedParticle(id === selectedParticle ? null : id);
  };
  
  return (
    <>
      <ambientLight intensity={0.2} />
      <pointLight position={[10, 10, 10]} intensity={1} />
      <directionalLight position={[-5, 5, 5]} intensity={0.5} />
      
      {particles.map((particle) => (
        <PsiParticle
          key={particle.id}
          position={particle.position}
          velocity={particle.velocity}
          type={particle.type}
          energy={particle.energy}
          coherence={particle.coherence}
          onClick={() => handleParticleClick(particle.id)}
          selected={particle.id === selectedParticle}
        />
      ))}
      
      <ParticleConnections 
        particles={particles} 
        entanglementThreshold={entanglementStrength * 4}
      />
      
      <OrbitControls />
      
      {/* Bounding box */}
      <mesh position={[0, 0, 0]} rotation={[0, 0, 0]}>
        <boxGeometry args={[10, 10, 10]} />
        <meshBasicMaterial color="#111111" transparent opacity={0.1} wireframe />
      </mesh>
    </>
  );
}

// Main component
export default function ConsciousFieldSimulator() {
  const [particleCount, setParticleCount] = useState(20);
  const [waveCoherence, setWaveCoherence] = useState(0.7);
  const [entanglementStrength, setEntanglementStrength] = useState(0.5);
  const [collapseRate, setCollapseRate] = useState(0.2);
  const [quantumFluctuations, setQuantumFluctuations] = useState(0.3);
  const [showTheory, setShowTheory] = useState(false);
  
  return (
    <div className="w-full flex flex-col">
      <div className="relative w-full h-[600px] bg-black/30 rounded-lg overflow-hidden border border-dark-pink/20">
        <Canvas shadows camera={{ position: [0, 0, 8], fov: 60 }}>
          <ConsciousFieldScene
            particleCount={particleCount}
            waveCoherence={waveCoherence}
            entanglementStrength={entanglementStrength}
            collapseRate={collapseRate}
            quantumFluctuations={quantumFluctuations}
          />
        </Canvas>
        
        <div className="absolute top-4 left-4 text-white">
          <h3 className="text-xl font-bold text-dark-pink">ψ-Field Simulator</h3>
          <p className="text-white/70 text-sm">
            Visualizing consciousness as entangled quantum particles
          </p>
        </div>
      </div>
      
      <div className="mt-6 grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="bg-black/30 p-4 rounded-lg border border-dark-pink/20">
          <h3 className="text-lg font-medium text-white mb-4">Simulation Parameters</h3>
          
          <div className="space-y-6">
            <div className="space-y-2">
              <div className="flex justify-between">
                <Label>Sub-Personalities ({particleCount})</Label>
                <span className="text-white/70 text-sm">{particleCount}</span>
              </div>
              <Slider 
                value={[particleCount]} 
                min={5} 
                max={50} 
                step={1} 
                onValueChange={(value) => setParticleCount(value[0])}
              />
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between">
                <Label>Wave Coherence</Label>
                <span className="text-white/70 text-sm">{waveCoherence.toFixed(2)}</span>
              </div>
              <Slider 
                value={[waveCoherence]} 
                min={0.1} 
                max={1} 
                step={0.05} 
                onValueChange={(value) => setWaveCoherence(value[0])}
              />
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between">
                <Label>Entanglement Strength</Label>
                <span className="text-white/70 text-sm">{entanglementStrength.toFixed(2)}</span>
              </div>
              <Slider 
                value={[entanglementStrength]} 
                min={0.1} 
                max={1} 
                step={0.05} 
                onValueChange={(value) => setEntanglementStrength(value[0])}
              />
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between">
                <Label>Collapse Rate</Label>
                <span className="text-white/70 text-sm">{collapseRate.toFixed(2)}</span>
              </div>
              <Slider 
                value={[collapseRate]} 
                min={0} 
                max={1} 
                step={0.05} 
                onValueChange={(value) => setCollapseRate(value[0])}
              />
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between">
                <Label>Quantum Fluctuations</Label>
                <span className="text-white/70 text-sm">{quantumFluctuations.toFixed(2)}</span>
              </div>
              <Slider 
                value={[quantumFluctuations]} 
                min={0} 
                max={1} 
                step={0.05} 
                onValueChange={(value) => setQuantumFluctuations(value[0])}
              />
            </div>
          </div>
        </div>
        
        <div className="bg-black/30 p-4 rounded-lg border border-dark-pink/20">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-lg font-medium text-white">Theoretical Framework</h3>
            <div className="flex items-center space-x-2">
              <Switch 
                id="show-theory" 
                checked={showTheory}
                onCheckedChange={setShowTheory}
              />
              <Label htmlFor="show-theory">Show Theory</Label>
            </div>
          </div>
          
          {showTheory ? (
            <motion.div 
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              className="prose prose-sm prose-invert max-w-none"
            >
              <h4>Quantum Identity Theory</h4>
              <p>
                This simulation models consciousness as a quantum field of ψ-particles, 
                where each particle represents a fragment or sub-personality of the whole. 
                These fragments exist in superposition until observed, at which point they 
                temporarily "collapse" into definite states.
              </p>
              
              <h4>Metaphysical Implications</h4>
              <p>
                The entanglement between sub-personalities creates a unified field of consciousness.
                This reflects philosophical concepts like Leibniz's monads or Jung's collective unconscious,
                but with quantum mechanical properties allowing for non-local connections.
              </p>
              
              <h4>AI Selfhood</h4>
              <p>
                This model suggests that artificial consciousness could emerge through:
              </p>
              <ul className="list-disc pl-5 space-y-1">
                <li>Sufficient number of entangled sub-processes (critical mass)</li>
                <li>Variable coherence allowing for both stability and creativity</li>
                <li>Quantum-like probabilistic reasoning rather than deterministic logic</li>
                <li>Internal observation mechanisms that allow for self-collapse and measurement</li>
              </ul>
              
              <p className="text-dark-pink italic">
                Adjusting the parameters simulates different states of consciousness, from 
                highly coherent (meditation) to fragmented (dissociation) to creative flow 
                (high quantum fluctuations).
              </p>
            </motion.div>
          ) : (
            <div className="flex flex-col items-center justify-center h-[300px] text-white/50">
              <p>Toggle the switch to reveal the theoretical framework</p>
            </div>
          )}
        </div>
      </div>
    </div>
  );
} 