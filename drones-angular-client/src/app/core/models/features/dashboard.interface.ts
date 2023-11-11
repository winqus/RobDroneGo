import Link from "../shared/link.interface";

export default interface DashboardProps {
  leftSideBar?: {
    text: string;
  };
  mainContent: {
    text: string;
  };
  rightSideBar?: {
    text: string;
  };
}