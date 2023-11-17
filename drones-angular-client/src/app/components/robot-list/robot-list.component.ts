import { Component, OnInit } from '@angular/core';
import Robot from 'src/app/core/models/robot.model';
import { RobotService } from 'src/app/services/robot.service';

@Component({
  selector: 'app-robot-list',
  templateUrl: './robot-list.component.html',
  styleUrls: ['./robot-list.component.css'],
})
export class RobotListComponent implements OnInit {
  robots: Robot[] = [];

  constructor(private robotService: RobotService) {}

  ngOnInit(): void {
    this.robotService.getRobot().subscribe((robots) => {
      this.robots = robots;
    });
  }
}
