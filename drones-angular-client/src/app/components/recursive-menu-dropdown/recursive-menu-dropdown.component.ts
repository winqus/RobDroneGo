import { Component, Input, OnInit } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { User } from '../../core/authentication/models/user.model';
import { UserService } from '../../core/authentication/services/user.service';
import DropdownLink from '../../core/models/shared/dropdownLink.interface';
import Link from '../../core/models/shared/link.interface';

@Component({
  selector: 'app-recursive-menu-dropdown',
  templateUrl: './recursive-menu-dropdown.component.html',
  styleUrls: ['./recursive-menu-dropdown.component.css'],
})
export class RecursiveMenuDropdownComponent implements OnInit {
  @Input() links?: Array<Link | DropdownLink>;
  filteredLinks$?: Observable<Array<Link | DropdownLink>>;

  constructor(private userService: UserService) {}

  ngOnInit(): void {
    this.filteredLinks$ = this.filterLinks(this.links);
  }

  isDropdownLink(link: Link | DropdownLink): link is DropdownLink {
    return 'dropdownLinks' in link;
  }

  filterLinks(links?: Array<Link | DropdownLink>): Observable<Link[]> {
    if (!links) {
      return of([]);
    }

    return this.userService.currentUser.pipe(
      catchError((error) => {
        console.error('Error fetching current user:', error);
        return of([]);
      }),
      map((user) => {
        if (!user) {
          console.error('Error: User role error');
          return [];
        }

        const role = (user as User).role;
        return links.filter((link) => !link.displayForRoles || link.displayForRoles.includes(role));
      }),
    );
  }
}
