import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import Elevator from 'src/app/core/models/elevator.model';
import { ElevatorService } from 'src/app/services/elevator.service';

@Component({
  selector: 'app-elevator-list',
  templateUrl: './elevator-list.component.html',
  styleUrls: ['./elevator-list.component.css'],
})
export class ElevatorListComponent implements OnInit {
  elevators: Elevator[] = [];
  constructor(
    private elevatorService: ElevatorService,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  ngOnInit(): void {
    const code = this.route.snapshot.paramMap.get('code') as string;
    this.elevatorService.getElevator(code).subscribe((elevators) => {
      if (elevators !== null) {
        this.elevators = Array.isArray(elevators) ? elevators : [elevators];
      }
    });
  }
}
