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

// Particle that follows the curved spacetime
function GravitationalParticle({ theory = 'gr' }) {
  const particleRef = useRef<THREE.Mesh>(null);
  const [position, setPosition] = useState({ x: 5, y: 0, z: 0 });
  const [velocity, setVelocity] = useState({ x: 0, y: 0, z: 0 });
  
  useFrame((state, delta) => {
    if (!particleRef.current) return;
    
    const { x, y, z } = position;
    const distance = Math.sqrt(x * x + z * z);
    
    // Calculate gravitational acceleration
    let acceleration;
    if (theory === 'gr') {
      acceleration = -1 / (distance * distance);
    } else if (theory === 'modified') {
      acceleration = -1 / Math.sqrt(distance);
    } else {
      acceleration = 0;
    }
    
    // Update velocity and position
    const newVelocity = {
      x: velocity.x + (acceleration * x / distance) * delta,
      y: velocity.y,
      z: velocity.z + (acceleration * z / distance) * delta
    };
    
    const newPosition = {
      x: position.x + newVelocity.x * delta,
      y: position.y,
      z: position.z + newVelocity.z * delta
    };
    
    setVelocity(newVelocity);
    setPosition(newPosition);
    particleRef.current.position.set(newPosition.x, newPosition.y, newPosition.z);
  });
  
  return (
    <Sphere ref={particleRef} args={[0.2, 16, 16]} position={[position.x, position.y, position.z]}>
      <meshStandardMaterial 
        color="#00ffff" 
        emissive="#00ffff"
        emissiveIntensity={0.5}
      />
    </Sphere>
  );
}

// Label for the current theory
function TheoryLabel({ position, text }: { position: [number, number, number], text: string }) {
  return (
    <Text
      position={position}
      color="white"
      fontSize={0.8}
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
      <ambientLight intensity={0.5} />
      <pointLight position={[10, 10, 10]} intensity={1} />
      <SpacetimeGrid mass={2} theory={theory} />
      <MassObject />
      <GravitationalParticle theory={theory} />
      <OrbitControls enableZoom={true} minDistance={5} maxDistance={30} />
      <TheoryLabel 
        position={[0, 5, 0]} 
        text={theory === 'gr' ? 'General Relativity' : 'Modified Gravity'} 
      />
    </>
  );
}

// Main component
export default function SpacetimeCurvature() {
  const [theory, setTheory] = useState<'gr' | 'modified'>('gr');
  
  return (
    <div className="w-full h-[400px]">
      <Canvas camera={{ position: [0, 10, 20], fov: 50 }}>
        <Scene theory={theory} />
      </Canvas>
      
      <div className="absolute bottom-4 left-4 right-4 p-4 bg-black/50 rounded-md flex justify-center gap-4">
        <button
          className={`px-4 py-2 rounded-md ${
            theory === 'gr' 
              ? 'bg-dark-pink text-white' 
              : 'bg-transparent text-dark-pink border border-dark-pink'
          }`}
          onClick={() => setTheory('gr')}
        >
          General Relativity
        </button>
        <button
          className={`px-4 py-2 rounded-md ${
            theory === 'modified' 
              ? 'bg-dark-pink text-white' 
              : 'bg-transparent text-dark-pink border border-dark-pink'
          }`}
          onClick={() => setTheory('modified')}
        >
          Modified Gravity
        </button>
      </div>
    </div>
  );
}
