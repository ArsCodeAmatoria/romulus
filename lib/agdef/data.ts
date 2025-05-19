import { parse } from 'papaparse';

export interface SimulationData {
  z: number;
  mu_agdef: number;
  mu_lcdm: number;
  dL_agdef: number;
  dL_lcdm: number;
}

export interface PantheonData {
  z: number;
  mu: number;
  mu_err: number;
}

export interface ISWData {
  l: number;
  cl_agdef: number;
  cl_planck: number;
  cl_err: number;
}

export async function loadSimulationData(): Promise<SimulationData[]> {
  try {
    const response = await fetch('/data/agdef_simulation.csv');
    const csvText = await response.text();
    const { data } = parse(csvText, {
      header: true,
      dynamicTyping: true,
    });
    return data as SimulationData[];
  } catch (error) {
    console.error('Error loading simulation data:', error);
    return [];
  }
}

export async function loadPantheonData(): Promise<PantheonData[]> {
  try {
    const response = await fetch('/data/pantheon_plus.csv');
    const csvText = await response.text();
    const { data } = parse(csvText, {
      header: true,
      dynamicTyping: true,
    });
    return data as PantheonData[];
  } catch (error) {
    console.error('Error loading Pantheon+ data:', error);
    return [];
  }
}

export async function loadISWData(): Promise<ISWData[]> {
  try {
    const response = await fetch('/data/planck_cmb_isw.csv');
    const csvText = await response.text();
    const { data } = parse(csvText, {
      header: true,
      dynamicTyping: true,
    });
    return data as ISWData[];
  } catch (error) {
    console.error('Error loading ISW data:', error);
    return [];
  }
} 