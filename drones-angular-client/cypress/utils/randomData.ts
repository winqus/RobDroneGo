export const randomBuildingCode = () => 'b' + Math.random().toString(36).substring(2, 6).toUpperCase();
export const randomFloorNumber = () => Math.floor(Math.random() * 1000);
export const randomName = () => 'name' + Math.random().toString(36).substring(2, 15).toUpperCase();
export const randomSerialNumber = () => 'serial' + Math.random().toString(36).substring(2, 20).toUpperCase();
