import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import Floor from 'src/app/core/models/floor.model';
import { FloorService } from 'src/app/services/floor.service';

@Component({
  selector: 'app-floors-served-by-elevator-list',
  templateUrl: './floors-served-by-elevator-list.component.html',
  styleUrls: ['./floors-served-by-elevator-list.component.css'],
})
export class FloorsServedByElevatorListComponent {
  floors: Floor[] = [];

  constructor(
    private floorService: FloorService,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  ngOnInit(): void {
    const code = this.route.snapshot.paramMap.get('code') as string;
    this.floorService.getAllFloors().subscribe((floors) => {
      this.floors = floors.filter((floor) => floor.servedByElevator);
    });
  }

  editFloor(floorData: Floor) {
    this.router.navigate(['campus/building', floorData.buildingCode, 'floors', floorData.floorNumber, 'edit'], { state: { data: floorData } });
  }
}
