'use client';

import { useRef, useState } from 'react';
import { Canvas, useFrame } from '@react-three/fiber';
import { Torus, Stars, OrbitControls } from '@react-three/drei';
import * as THREE from 'three';

function TorusModel() {
  const torusRef = useRef<THREE.Mesh>(null);
  
  useFrame((_, delta) => {
    if (torusRef.current) {
      torusRef.current.rotation.x += delta * 0.2;
      torusRef.current.rotation.y += delta * 0.3;
    }
  });

  return (
    <Torus 
      ref={torusRef} 
      args={[4, 1.5, 32, 100]} 
      position={[0, 0, 0]}
    >
      <meshStandardMaterial 
        color="#ff007f" 
        wireframe={true} 
        emissive="#ff007f"
        emissiveIntensity={0.5}
      />
    </Torus>
  );
}

export default function SpaceScene() {
  return (
    <div className="w-full h-full absolute inset-0">
      <Canvas camera={{ position: [0, 0, 15] }}>
        <ambientLight intensity={0.2} />
        <pointLight position={[10, 10, 10]} intensity={1} color="#ff007f" />
        <pointLight position={[-10, -10, -10]} intensity={0.5} color="#ff007f" />
        <TorusModel />
        <Stars 
          radius={100} 
          depth={50} 
          count={5000} 
          factor={4} 
          saturation={0} 
          fade 
          speed={1}
        />
        <OrbitControls 
          enableZoom={false} 
          enablePan={false} 
          autoRotate 
          autoRotateSpeed={0.5} 
        />
      </Canvas>
    </div>
  );
} 