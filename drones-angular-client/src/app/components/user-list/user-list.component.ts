import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { User } from '../../core/authentication/models/user.model';
import { UserService } from '../../core/authentication/services/user.service';

@Component({
  selector: 'app-user-list',
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.css'],
})
export class UserListComponent implements OnInit {
  constructor(private userService: UserService) {}

  users$: Observable<User[]> | undefined;

  ngOnInit(): void {
    this.users$ = this.userService.getAllUsers();
  }

  onConfirmToggle(user: User): void {
    const newStatus = !user.isConfirmed;
    this.userService.confirmUser(user.email, newStatus).subscribe(() => {
      user.isConfirmed = newStatus;
    });
  }
}
