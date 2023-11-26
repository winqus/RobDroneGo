import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import Floor from 'src/app/core/models/floor.model';
import { FloorService } from 'src/app/services/floor.service';

@Component({
  selector: 'app-floor-list',
  templateUrl: './floor-list.component.html',
  styleUrls: ['./floor-list.component.css'],
})
export class FloorListComponent implements OnInit {
  floorsData: Floor[] = [];
  floors: Floor[] = [];
  showOnlyServedByElevator = false;

  constructor(
    private floorService: FloorService,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  ngOnInit(): void {
    const code = this.route.snapshot.paramMap.get('code') as string;
    this.floorService.getFloorsByBuildingCode(code).subscribe((floors) => {
      this.floorsData = floors;
      this.floors = this.floorsData;
    });
  }

  editFloor(floorData: Floor) {
    this.router.navigate(['campus/building', floorData.buildingCode, 'floors', floorData.floorNumber, 'edit'], { state: { data: floorData } });
  }

  onElevatorToggle(event: Event): void {
    const isChecked = (event.target as HTMLInputElement).checked;
    // Add your logic here that should run when the checkbox is toggled
    console.log(`Checkbox is ${isChecked ? 'checked' : 'not checked'}`);
    this.floors = this.floorsData.filter((floor: Floor) => (isChecked ? floor.servedByElevator : true));
  }
}
