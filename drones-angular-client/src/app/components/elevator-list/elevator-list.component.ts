import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import Elevator from '../../core/models/elevator.model';
import { ElevatorService } from '../../services/elevator.service';

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
  editElevator(elevator: Elevator) {
    const code = this.route.snapshot.paramMap.get('code') as string;
    elevator.buildingCode = code;
    this.router.navigate(['campus/building', code, 'elevator', 1, 'edit'], { state: { data: elevator } });
  }
}
