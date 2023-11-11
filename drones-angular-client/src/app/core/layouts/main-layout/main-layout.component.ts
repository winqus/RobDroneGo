import { Component } from '@angular/core';
import { TEXT_TOKENS as content } from '../../../../assets/i18n/_textTokens';

@Component({
  selector: 'app-main-layout',
  templateUrl: './main-layout.component.html',
  styleUrls: ['./main-layout.component.css']
})
export class MainLayoutComponent {
  navbarData = content.components.navbar;
  footerData = content.components.footer;
}
