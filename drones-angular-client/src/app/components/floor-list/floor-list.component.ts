import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import Floor from 'src/app/core/models/floor.model';
import { FloorService } from 'src/app/services/floor.service';

@Component({
  selector: 'app-floor-list',
  templateUrl: './floor-list.component.html',
  styleUrls: ['./floor-list.component.css']
})
export class FloorListComponent implements OnInit{

  floors: Floor[] = [];
  constructor(private floorService: FloorService, private route: ActivatedRoute) { }

  ngOnInit(): void {
    const code = this.route.snapshot.paramMap.get('code') as string;
    this.floorService.getFloorsByBuildingCode(code).subscribe((floors) => {
      this.floors = floors;
    });
  }

}
