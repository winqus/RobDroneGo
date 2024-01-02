import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { UserService } from '../../core/authentication/services/user.service';

@Component({
  template: '',
})
export class LogoutComponent implements OnInit {
  constructor(
    private userService: UserService,
    private router: Router,
  ) {}

  ngOnInit() {
    this.userService.logout();
    this.router.navigate(['/']);
  }
}
