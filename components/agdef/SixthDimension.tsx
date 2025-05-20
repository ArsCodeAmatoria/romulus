'use client';

import React, { useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import { Slider } from "@/components/ui/slider";
import { Label } from "@/components/ui/label";
import { Canvas } from '@react-three/fiber';
import { OrbitControls } from '@react-three/drei';
import * as THREE from 'three';

// Visualization of the 6D theory with interactive elements
const SixDVisualizer = () => {
  const [coupling56, setCoupling56] = useState(0.5);
  const [dimension6Strength, setDimension6Strength] = useState(0.7);
  const [entropyFlow, setEntropyFlow] = useState(0.3);
  
  return (
    <div className="space-y-6">
      <div className="h-60 bg-zinc-900/50 rounded-md overflow-hidden">
        <Canvas camera={{ position: [5, 5, 5], fov: 50 }}>
          <ambientLight intensity={0.5} />
          <pointLight position={[10, 10, 10]} intensity={1} />
          <BraneVisualization 
            coupling56={coupling56}
            dimension6Strength={dimension6Strength}
            entropyFlow={entropyFlow}
          />
          <OrbitControls enableZoom={true} minDistance={3} maxDistance={10} />
          <gridHelper args={[10, 10, '#444', '#222']} />
        </Canvas>
      </div>
      
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="space-y-2">
          <Label htmlFor="coupling-slider" className="text-white">5D-6D Coupling Strength: {coupling56.toFixed(2)}</Label>
          <Slider 
            id="coupling-slider"
            min={0}
            max={1}
            step={0.01} 
            value={[coupling56]}
            onValueChange={(values) => setCoupling56(values[0])}
          />
          <p className="text-xs text-white/60">Controls how 5D anti-gravity interacts with 6D tensor field</p>
        </div>
        
        <div className="space-y-2">
          <Label htmlFor="dimension6-slider" className="text-white">6D Field Strength: {dimension6Strength.toFixed(2)}</Label>
          <Slider 
            id="dimension6-slider"
            min={0}
            max={1}
            step={0.01} 
            value={[dimension6Strength]}
            onValueChange={(values) => setDimension6Strength(values[0])}
          />
          <p className="text-xs text-white/60">Magnitude of information field curvature</p>
        </div>
        
        <div className="space-y-2">
          <Label htmlFor="entropy-slider" className="text-white">Entropy Flow Rate: {entropyFlow.toFixed(2)}</Label>
          <Slider 
            id="entropy-slider"
            min={0}
            max={1}
            step={0.01} 
            value={[entropyFlow]}
            onValueChange={(values) => setEntropyFlow(values[0])}
          />
          <p className="text-xs text-white/60">Rate of information transfer across dimensions</p>
        </div>
      </div>
    </div>
  );
};

// Three.js component for the visualization
interface BraneVisualizationProps {
  coupling56: number;
  dimension6Strength: number;
  entropyFlow: number;
}

const BraneVisualization: React.FC<BraneVisualizationProps> = ({ 
  coupling56, 
  dimension6Strength, 
  entropyFlow 
}) => {
  // 3D brane representation with deformations based on parameters
  return (
    <>
      {/* Central 3D brane representation */}
      <mesh>
        <sphereGeometry args={[2, 32, 32]} />
        <meshStandardMaterial 
          color="#ff007f" 
          wireframe={true}
          transparent={true}
          opacity={0.6}
        />
      </mesh>
      
      {/* 5D field representation */}
      <mesh>
        <torusGeometry args={[3, 0.2, 16, 100]} />
        <meshStandardMaterial 
          color="#00ffff" 
          emissive="#00ffff"
          emissiveIntensity={0.5 * dimension6Strength}
        />
      </mesh>
      
      {/* 6D field representation */}
      <mesh rotation={[Math.PI/2, 0, 0]}>
        <torusGeometry args={[3.5, 0.15, 16, 100]} />
        <meshStandardMaterial 
          color="#ffff00" 
          emissive="#ffff00"
          emissiveIntensity={0.5 * coupling56}
        />
      </mesh>
      
      {/* Energy flow lines based on coupling */}
      {Array.from({ length: 8 }).map((_, i) => (
        <mesh key={i} position={[
          Math.sin(i * Math.PI/4) * 2.5, 
          Math.cos(i * Math.PI/4) * 2.5 * entropyFlow, 
          0
        ]}>
          <sphereGeometry args={[0.1, 8, 8]} />
          <meshStandardMaterial 
            color="#ffffff" 
            emissive="#ffffff"
            emissiveIntensity={0.8 * entropyFlow}
          />
        </mesh>
      ))}
    </>
  );
};

// Code example component
const HaskellExample = () => {
  return (
    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm font-mono">
      <pre className="text-white/80">
{`-- Simplified 6D curvature matrix (embedding energy + coupling)
type CurvatureTensor6D = Matrix Double

curvature6D :: CurvatureTensor6D
curvature6D = (6><6)
  [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.3
  , 0.0, 1.0, 0.0, 0.0, 0.2, 0.2
  , 0.0, 0.0, 1.0, 0.0, 0.3, 0.1
  , 0.0, 0.0, 0.0, 1.0, 0.0, 0.4
  , 0.1, 0.2, 0.3, 0.0, 0.7, 0.5
  , 0.3, 0.2, 0.1, 0.4, 0.5, 1.3 ]

-- Extract coupling between 5th and 6th dimensions
coupling56 :: Double
coupling56 = curvature6D \`atIndex\` (4, 5)

-- Calculate entropy flow based on coupling
entropyFlow :: Double -> Double -> Double
entropyFlow coupling strength = 
  tanh (coupling * strength) * exp (-coupling / strength)

-- Biological aging acceleration factor
agingFactor :: Double -> Double -> Double -> Double
agingFactor coupling dimension6 entropy =
  1.0 + (coupling * dimension6 * entropy)`}
      </pre>
    </div>
  );
};

// Main component
export function SixthDimension() {
  return (
    <div className="space-y-6">
      <Card className="bg-black border-dark-pink/20">
        <CardHeader>
          <CardTitle className="text-white">The 6th Dimension: Information Coupling Field</CardTitle>
          <CardDescription className="text-white/70">
            Extending AGDEF theory into information geometry
          </CardDescription>
        </CardHeader>
        <CardContent className="text-white/80 space-y-6">
          <p>
            Building on the 5-dimensional AGDEF theory, we propose a 6th dimension that functions as a 
            coupling field for information geometry and tensor interaction. While the 5th dimension encodes 
            anti-gravity and dark energy effects, the 6th dimension governs information exchange, quantum 
            entanglement, and field coupling across spacetime.
          </p>
          
          <div className="bg-zinc-900/50 p-4 rounded-md">
            <h3 className="text-lg font-medium text-dark-pink mb-2">Hierarchy of Dimensions in AGDEF Theory</h3>
            <div className="overflow-x-auto">
              <table className="w-full border-collapse">
                <thead>
                  <tr className="border-b border-dark-pink/20">
                    <th className="text-left p-2 font-medium text-dark-pink">Dimension</th>
                    <th className="text-left p-2 font-medium text-dark-pink">Physical Role</th>
                    <th className="text-left p-2 font-medium text-dark-pink">AGDEF Interpretation</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-pink/10">
                  <tr>
                    <td className="p-2">1–3</td>
                    <td className="p-2">Spatial dimensions</td>
                    <td className="p-2">Observable physical space (x, y, z)</td>
                  </tr>
                  <tr>
                    <td className="p-2">4</td>
                    <td className="p-2">Time</td>
                    <td className="p-2">Evolves state tensors and entropy</td>
                  </tr>
                  <tr>
                    <td className="p-2">5</td>
                    <td className="p-2">Anti-gravity energy field</td>
                    <td className="p-2">Source of curvature repulsion (dark energy)</td>
                  </tr>
                  <tr>
                    <td className="p-2">6</td>
                    <td className="p-2">Tensor field coupling</td>
                    <td className="p-2">Governs interactions, entanglement, entropy flow</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">6D Mathematical Framework</h3>
            <p className="mb-4">
              We extend the metric to a 6D manifold, introducing new terms that capture the interaction 
              between standard spacetime, the anti-gravity field, and the information coupling field:
            </p>
            <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
              <DisplayMath math="g_{AB}, A,B=0,1,2,3,4,5" />
            </div>
            <p className="mt-2">Where:</p>
            <ul className="list-disc pl-6 space-y-1 mt-2">
              <li><InlineMath math="g_{55}" /> is the anti-gravity (AGDEF) component</li>
              <li><InlineMath math="g_{56}" /> is the cross-term that controls field interference</li>
              <li><InlineMath math="g_{66}" /> introduces internal field symmetry metrics</li>
            </ul>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">Energy Field Embedding</h3>
            <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
              <DisplayMath math="T_{AB} = \begin{bmatrix} T_{\mu\nu} & A_{5\mu} & Q_{6\mu} \\ A_{\mu5} & A_{55} & \chi_{56} \\ Q_{6\mu} & \chi_{65} & \Omega_{66} \end{bmatrix}" />
            </div>
            <p className="mt-2">Where:</p>
            <ul className="list-disc pl-6 space-y-1 mt-2">
              <li><InlineMath math="Q_{6\mu}" />: energy vector components tied to information transfer</li>
              <li><InlineMath math="\chi_{56}" />: coupling term between AGDEF and 6th field</li>
              <li><InlineMath math="\Omega_{66}" />: "field entanglement curvature," defines local state decoherence</li>
            </ul>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">Physical Interpretations of the 6th Dimension</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mt-2">
              <div className="bg-zinc-900/30 p-4 rounded-md">
                <h4 className="font-medium text-dark-pink">Quantum Entanglement Medium</h4>
                <p className="text-sm mt-1">
                  Explains how particles maintain coherence over distance. Curvature in the 6th dimension 
                  encodes entanglement structure.
                </p>
              </div>
              <div className="bg-zinc-900/30 p-4 rounded-md">
                <h4 className="font-medium text-dark-pink">Field Coupling Space</h4>
                <p className="text-sm mt-1">
                  Describes how energy fields (gravitational, electromagnetic) interact. Could unify gauge 
                  symmetries (SU(3), SU(2), U(1)) with spacetime geometry.
                </p>
              </div>
              <div className="bg-zinc-900/30 p-4 rounded-md">
                <h4 className="font-medium text-dark-pink">Information Geometry</h4>
                <p className="text-sm mt-1">
                  The 6th axis encodes entropy gradients and data flow. Biological aging, memory, and 
                  complexity may evolve along this dimension.
                </p>
              </div>
              <div className="bg-zinc-900/30 p-4 rounded-md">
                <h4 className="font-medium text-dark-pink">String Tension or Brane Flux</h4>
                <p className="text-sm mt-1">
                  In string/brane theory, higher-dimensional tension fields connect compactified geometries. 
                  The 6th dimension could store string potential states.
                </p>
              </div>
            </div>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">Visualization & Simulation</h3>
            <p className="mb-4">
              The interactive visualization below demonstrates the coupling between the 5th and 6th dimensions,
              showing how energy and information flow across dimensions and affect the observed 3D brane:
            </p>
            <SixDVisualizer />
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">Computational Implementation (Haskell)</h3>
            <p className="mb-4">
              This Haskell implementation models the 6D curvature tensor and calculates coupling effects:
            </p>
            <HaskellExample />
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">Implications for Aging & Entropy</h3>
            <p>
              The 6th dimension offers a novel perspective on biological aging. If entropy increase in biological
              systems is partially driven by coupling with the 6th dimension field, manipulating this coupling
              could theoretically affect aging rates. The model suggests that:
            </p>
            <ul className="list-disc pl-6 space-y-1 mt-2">
              <li>Aging acceleration correlates with local 6D field strength</li>
              <li>Entropy flow is modulated by 5D-6D coupling strength</li>
              <li>Regions with strong anti-gravity (5D) may experience different aging rates due to 6D coupling</li>
              <li>Theoretically, manipulating 6D curvature could reduce entropy accumulation in biological systems</li>
            </ul>
          </div>
          
          <div>
            <h3 className="text-lg font-medium text-dark-pink mb-2">Summary & Next Steps</h3>
            <p>
              The 6-dimensional AGDEF theory extends our understanding of dark energy and cosmic acceleration
              into the domain of information physics. By coupling the 5th dimension (anti-gravity) with a 6th
              dimension (information geometry), we provide a unified framework for understanding gravity,
              dark energy, entanglement, and entropy flow.
            </p>
            <p className="mt-2">
              Future research will focus on developing testable predictions from this model, particularly
              in the realms of cosmic microwave background anisotropies, entanglement preservation at macroscopic
              scales, and potential applications to entropy manipulation in biological systems.
            </p>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

export default SixthDimension; 