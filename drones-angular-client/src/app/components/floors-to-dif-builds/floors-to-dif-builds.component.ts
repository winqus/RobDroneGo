import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Observable } from 'rxjs/internal/Observable';
import Floor from 'src/app/core/models/floor.model';
import { PassageService } from 'src/app/services/passage.service';

@Component({
  selector: 'app-floors-to-dif-builds',
  templateUrl: './floors-to-dif-builds.component.html',
  styleUrls: ['./floors-to-dif-builds.component.css'],
})
export class FloorsToDifBuildsComponent implements OnInit {
  floorsData: Floor[] = [];

  constructor(
    private route: ActivatedRoute,
    private passageService: PassageService,
  ) {}

  ngOnInit() {
    const buildingCode = this.route.snapshot.paramMap.get('code') as string;
    this.passageService.listFloorsWithPassagesToDifferentBuilding(buildingCode).subscribe((floors) => {
      this.floorsData = floors;
    });
  }
}
