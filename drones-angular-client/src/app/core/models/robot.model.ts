export default interface Robot{
    id: string;
    code: string;
    description: string | null;
    nickname: string;
    serialNumber: string;
    available: boolean;
    type: string;
}