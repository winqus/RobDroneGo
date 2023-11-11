import { Component, Input } from '@angular/core';
import Link from 'src/app/core/models/shared/link.interface';

interface NavbarProps {
  title: string;
  links?: Link[];
  profileDropdown: {
    name: string;
    links?: Link[];
  };
}

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.css']
})
export class NavbarComponent {
  @Input() props: NavbarProps = this.getDefaultProps();

  getDefaultProps(): NavbarProps {
    return {
      title: "No props",
      profileDropdown: {
        name: "No props"
      }
    }
  }
}
