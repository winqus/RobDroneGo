import { Component, Input, OnInit } from '@angular/core';
import { User } from 'src/app/core/authentication/models/user.model';
import { UserService } from 'src/app/core/authentication/services/user.service';
import DropdownLink from 'src/app/core/models/shared/dropdownLink.interface';
import Link from 'src/app/core/models/shared/link.interface';

interface NavbarProps {
  title: string;
  links?: Array<Link | DropdownLink>;
  profileDropdown: {
    name: string;
    links?: Link[];
  };
}

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.css'],
})
export class NavbarComponent implements OnInit {
  @Input() props: NavbarProps = this.getDefaultProps();

  constructor(private userService: UserService) {}

  ngOnInit() {
    this.initializeProps();
  }

  initializeProps() {
    this.userService.currentUser.subscribe((currentUser: User | null) => {
      if (currentUser) {
        this.props = {
          ...this.props,
          profileDropdown: {
            name: currentUser.firstName,
            links: this.props?.profileDropdown?.links || [],
          },
        };
      }
    });
  }

  getDefaultProps(): NavbarProps {
    return {
      title: 'No props',
      profileDropdown: {
        name: 'No props',
      },
    };
  }

  isDropdownLink(link: any): link is DropdownLink {
    return 'dropdownLinks' in link && link.dropdownLinks.length > 0;
  }

  isSimpleLink(link: any): boolean {
    return !('dropdownLinks' in link);
  }
}
