'use client';

import { useRef, useState, useEffect } from 'react';
import { Canvas, useFrame, useThree } from '@react-three/fiber';
import { OrbitControls, Sphere, Plane, Text } from '@react-three/drei';
import * as THREE from 'three';

// Grid that will be deformed to represent curved spacetime
function SpacetimeGrid({ mass = 1, theory = 'gr' }) {
  const meshRef = useRef<THREE.Mesh>(null);
  const gridSize = 20;
  const gridDivisions = 30;
  
  // Generate grid geometry with vertices that will be displaced
  useEffect(() => {
    if (!meshRef.current) return;
    
    const geometry = meshRef.current.geometry as THREE.PlaneGeometry;
    // Reset all vertices to flat plane
    for (let i = 0; i < geometry.attributes.position.count; i++) {
      const x = geometry.attributes.position.getX(i);
      const y = geometry.attributes.position.getY(i);
      geometry.attributes.position.setZ(i, 0);
    }
    
    geometry.computeVertexNormals();
    geometry.attributes.position.needsUpdate = true;
  }, [theory]); // Re-initialize when theory changes
  
  // Deform grid based on selected theory
  useFrame(() => {
    if (!meshRef.current) return;
    
    const geometry = meshRef.current.geometry as THREE.PlaneGeometry;
    
    // Apply gravitational deformation to the grid
    for (let i = 0; i < geometry.attributes.position.count; i++) {
      const x = geometry.attributes.position.getX(i);
      const y = geometry.attributes.position.getY(i);
      const distance = Math.sqrt(x * x + y * y);
      
      // Avoid division by zero at the center
      if (distance < 0.1) continue;
      
      let curvature;
      if (theory === 'gr') {
        // Standard GR curvature: inversely proportional to distance
        curvature = -mass / distance;
      } else if (theory === 'modified') {
        // Modified gravity: stronger effect at large distances
        curvature = -mass / Math.sqrt(distance);
      } else {
        curvature = 0;
      }
      
      // Limit curvature to a reasonable range
      curvature = Math.max(curvature, -3);
      
      geometry.attributes.position.setZ(i, curvature);
    }
    
    geometry.computeVertexNormals();
    geometry.attributes.position.needsUpdate = true;
  });
  
  return (
    <mesh ref={meshRef} rotation={[-Math.PI / 2, 0, 0]}>
      <planeGeometry args={[gridSize, gridSize, gridDivisions, gridDivisions]} />
      <meshStandardMaterial 
        color="#ff007f" 
        wireframe={true} 
        side={THREE.DoubleSide}
        emissive="#ff007f"
        emissiveIntensity={0.3}
      />
    </mesh>
  );
}

// Central mass object that deforms spacetime
function MassObject() {
  return (
    <Sphere args={[0.7, 32, 32]} position={[0, 0, 0]}>
      <meshStandardMaterial 
        color="#ff007f" 
        emissive="#ff007f"
        emissiveIntensity={0.8}
      />
    </Sphere>
  );
}

// Particle moving in the gravitational field
function GravitationalParticle({ theory = 'gr' }) {
  const meshRef = useRef<THREE.Mesh>(null);
  const orbitRadius = 8;
  const orbitSpeed = 0.3;
  const [angle, setAngle] = useState(0);
  
  useFrame((_, delta) => {
    if (!meshRef.current) return;
    
    // Update orbit angle - slower at greater distances in GR
    let speedFactor = 1;
    if (theory === 'gr') {
      // Standard relativistic orbital velocity decrease
      speedFactor = 1 / Math.sqrt(orbitRadius);
    } else if (theory === 'modified') {
      // Modified gravity: flatter rotation curve
      speedFactor = 0.9; // Nearly constant velocity regardless of radius
    }
    
    const newAngle = angle + delta * orbitSpeed * speedFactor;
    setAngle(newAngle);
    
    // Calculate position on orbit
    const x = Math.cos(newAngle) * orbitRadius;
    const y = Math.sin(newAngle) * orbitRadius;
    
    // Find height on the deformed grid (simplified approximation)
    let z;
    if (theory === 'gr') {
      z = -1 / Math.sqrt(x * x + y * y);
    } else if (theory === 'modified') {
      z = -1 / Math.pow(x * x + y * y, 0.25);
    } else {
      z = 0;
    }
    
    // Limit z to a reasonable range
    z = Math.max(z, -2);
    
    meshRef.current.position.set(x, y, z);
  });
  
  return (
    <Sphere ref={meshRef} args={[0.3, 16, 16]} position={[orbitRadius, 0, 0]}>
      <meshStandardMaterial 
        color="#ffffff" 
        emissive="#ffffff"
        emissiveIntensity={0.5}
      />
    </Sphere>
  );
}

// Label component for the two theories
function TheoryLabel({ position, text }: { position: [number, number, number], text: string }) {
  return (
    <Text
      position={position}
      fontSize={0.7}
      color="#ffffff"
      anchorX="center"
      anchorY="middle"
    >
      {text}
    </Text>
  );
}

// Main scene component
function Scene({ theory = 'gr' }) {
  return (
    <>
      <ambientLight intensity={0.2} />
      <pointLight position={[10, 10, 10]} intensity={1} color="#ffffff" />
      <SpacetimeGrid theory={theory} mass={1} />
      <MassObject />
      <GravitationalParticle theory={theory} />
      <OrbitControls 
        enablePan={false}
        maxPolarAngle={Math.PI / 2}
        minDistance={5}
        maxDistance={30}
      />
    </>
  );
}

// Main component with theory selection
export default function SpacetimeCurvature() {
  const [theory, setTheory] = useState<'gr' | 'modified'>('gr');
  
  return (
    <div className="w-full h-60 relative">
      <div className="absolute bottom-4 right-4 z-10 flex space-x-2">
        <button 
          onClick={() => setTheory('gr')}
          className={`px-3 py-1 text-xs rounded-full ${theory === 'gr' ? 'bg-dark-pink text-white' : 'bg-zinc-800 text-white/70'}`}
        >
          General Relativity
        </button>
        <button 
          onClick={() => setTheory('modified')}
          className={`px-3 py-1 text-xs rounded-full ${theory === 'modified' ? 'bg-dark-pink text-white' : 'bg-zinc-800 text-white/70'}`}
        >
          Modified Gravity
        </button>
      </div>
      
      <Canvas camera={{ position: [15, 15, 10], fov: 40 }}>
        <Scene theory={theory} />
      </Canvas>
    </div>
  );
} 