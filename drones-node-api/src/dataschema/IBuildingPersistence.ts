export interface IBuildingPersistence {
  id: string;
  name: string | null;
  code: string;
  description: string;
  floorSizeLength: number;
  floorSizeWidth: number;
  elevator?: {
    id: string;
    number: number;
    make?: string;
    model?: string;
    serialNumber?: string;
    description?: string;
  };
}
