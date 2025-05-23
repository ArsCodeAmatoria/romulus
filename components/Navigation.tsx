'use client';

import * as React from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { Leaf } from 'lucide-react';

import {
  NavigationMenu,
  NavigationMenuContent,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  NavigationMenuTrigger,
} from '@/components/ui/navigation-menu';

export default function Navigation() {
  const pathname = usePathname();
  console.log("Current pathname:", pathname);
  
  return (
    <div className="fixed top-0 left-0 right-0 z-50 bg-black/20 backdrop-blur-md border-b border-dark-pink/20">
      <div className="container flex h-16 items-center justify-between px-4">
        <NavigationMenu>
          <NavigationMenuList className="space-x-4">
            <NavigationMenuItem>
              <Link 
                href="/" 
                className={`text-xl font-bold neon-glow ${pathname === '/' ? 'text-dark-pink' : 'text-white'}`}
              >
                ROMULUS
              </Link>
            </NavigationMenuItem>
            
            <NavigationMenuItem>
              <NavigationMenuTrigger className={`${pathname.startsWith('/docs') || pathname === '/agdef' || pathname === '/seventh-dimension' || pathname === '/unified-consciousness' || pathname === '/unified-physics-consciousness' || pathname === '/eighth-dimension' || pathname === '/ninth-dimension' || pathname === '/tenth-dimension' || pathname === '/tenth-door' || pathname === '/dimension-zero' || pathname === '/dream-traversal' ? 'text-dark-pink' : 'text-white'}`}>
                Theories
              </NavigationMenuTrigger>
              <NavigationMenuContent>
                <ul className="grid w-[400px] gap-3 p-4 md:w-[900px] md:grid-cols-4">
                  <li>
                    <Link 
                      href="/agdef" 
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">AGDEF Theory</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        5D curvature projection and dark energy field effects
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/general-relativity" 
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">General Relativity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Tensor modifications to Einstein's field equations
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/emergent-gravity"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">Emergent Gravity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Verlinde's theory of gravity as an entropic force
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/mond"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">MOND & TeVeS</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Modified Newtonian Dynamics and Tensor-Vector-Scalar gravity
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/scalar-field"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">Scalar Field Gravity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        f(R) and scalar field approaches to modified gravity
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/docs/quantum-gravity"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none">Quantum Gravity</div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Holographic principle and quantum approaches to gravity
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/seventh-dimension"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        7th Dimension
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Meta-curvature and the configuration space of all possible universes
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/unified-consciousness"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        Unified Consciousness
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Mathematical formalization of reality as projection of consciousness
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/unified-physics-consciousness"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        Unified Physics & Consciousness
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Merging infinite consciousness with anti-gravity dark matter theory
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/eighth-dimension"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        The 8th Dimension
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        The Field of All Possible Consciousnesses
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/ninth-dimension"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        The 9th Dimension
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        The Meta-Law Field: Source code of consciousness and reality
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/tenth-dimension"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        The 10th Dimension
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        The Singularity of Totality: Where all distinctions dissolve
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/tenth-door"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        The 10th Door
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        A Mystical Treatise on the Singularity of All Being
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/dimension-zero"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        Dimension 0
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        The First Flicker: The Origin Point of All Dimensions
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/dream-traversal"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        Dream Traversal
                        <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Simulation of consciousness traversing 6D-8D dream space
                      </p>
                    </Link>
                  </li>
                  <li>
                    <Link 
                      href="/dream-consciousness"
                      className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-dark-pink/10 hover:text-dark-pink"
                    >
                      <div className="text-sm font-medium leading-none flex items-center">
                        Dream Consciousness
                      </div>
                      <p className="line-clamp-2 text-sm leading-snug text-white/70">
                        Explorations of dreams and consciousness
                      </p>
                    </Link>
                  </li>
                </ul>
              </NavigationMenuContent>
            </NavigationMenuItem>
            
            <NavigationMenuItem>
              <Link 
                href="/docs" 
                className={`${pathname === '/docs' ? 'text-dark-pink' : 'text-white'}`}
              >
                Documentation
              </Link>
            </NavigationMenuItem>
            
            <NavigationMenuItem>
              <Link 
                href="/interactive-demo" 
                className={`${pathname === '/interactive-demo' ? 'text-dark-pink' : 'text-white'} flex items-center`}
              >
                <span>Interactive</span>
                <span className="ml-1.5 px-1.5 py-0.5 text-xs bg-dark-pink/20 rounded text-dark-pink">New</span>
              </Link>
            </NavigationMenuItem>
          </NavigationMenuList>
        </NavigationMenu>
        
        <Link 
          href="https://arcana-obscura.vercel.app/"
          className="text-white hover:text-dark-pink transition-colors"
          aria-label="Arcana Obscura"
          target="_blank"
          rel="noopener noreferrer"
        >
          <Leaf size={24} />
        </Link>
      </div>
    </div>
  );
} 