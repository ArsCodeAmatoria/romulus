'use client';

import * as React from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { Popsicle } from 'lucide-react';

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
              <NavigationMenuTrigger className={`${pathname.startsWith('/docs') || pathname === '/agdef' || pathname === '/seventh-dimension' || pathname === '/unified-consciousness' || pathname === '/unified-physics-consciousness' || pathname === '/eighth-dimension' ? 'text-dark-pink' : 'text-white'}`}>
                Theories
              </NavigationMenuTrigger>
              <NavigationMenuContent>
                <ul className="grid w-[400px] gap-3 p-4 md:w-[500px] md:grid-cols-2">
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
          href="/dream-consciousness"
          className={`text-white hover:text-dark-pink transition-colors ${pathname === '/dream-consciousness' ? 'text-dark-pink' : 'text-white'}`}
          aria-label="Dream Consciousness Theory"
        >
          <Popsicle size={24} />
        </Link>
      </div>
    </div>
  );
} 