import { Component, Input } from '@angular/core';
import DropdownLink from 'src/app/core/models/shared/dropdownLink.interface';
import Link from 'src/app/core/models/shared/link.interface';

@Component({
  selector: 'app-recursive-menu-dropdown',
  templateUrl: './recursive-menu-dropdown.component.html',
  styleUrls: ['./recursive-menu-dropdown.component.css'],
})
export class RecursiveMenuDropdownComponent {
  @Input() links?: Array<Link | DropdownLink>;

  isDropdownLink(link: Link | DropdownLink): link is DropdownLink {
    return 'dropdownLinks' in link;
  }
}
