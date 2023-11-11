import { Component, Input } from '@angular/core';

interface FooterProps {
  text: string;
}

@Component({
  selector: 'app-footer',
  templateUrl: './footer.component.html',
  styleUrls: ['./footer.component.css']
})
export class FooterComponent {
  @Input() props: FooterProps = this.getDefaultProps();

  getDefaultProps(): FooterProps {
    return {
      text: "No props"
    }
  }
}
