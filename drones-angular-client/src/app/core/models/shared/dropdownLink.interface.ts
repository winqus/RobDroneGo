import Link from './link.interface';

export default interface DropdownLink extends Link {
  dropdownLinks?: Array<Link | DropdownLink>;
}
