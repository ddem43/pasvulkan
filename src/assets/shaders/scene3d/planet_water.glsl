#ifndef PLANET_WATER_GLSL
#define PLANET_WATER_GLSL

#if 1
// This solveQuadraticRoots function offers a significant improvement over the old solveQuadraticRoots
// function in terms of numerical stability and accuracy. In computing, especially for floating-point
// numbers, the representation and precision of real numbers are limited, which can lead to issues
// like loss of significance and catastrophic cancellation. This problem is particularly acute when
// dealing with values that are very close in magnitude but have opposite signs.
// The old solveQuadraticRoots function uses a direct approach to calculate the roots of the quadratic
// equation, which suffers from these numerical stability issues. Specifically, when 'b' and the
// square root of the discriminant ('d') in the quadratic formula have values close to each other
// but opposite in sign, it can lead to significant errors due to rounding and cancellation.
// This solveQuadraticRoots function addresses this by using an alternative formulation:
// q = -0.5 * (b + sign(b) * sqrt(b^2 - 4ac))
// t0 = q / a
// t1 = c / q
// This approach ensures that the terms added to compute 'q' always have the same sign, thus avoiding
// the catastrophic cancellation that can occur in the OldSolveQuadraticRoots function. By doing so,
// this solveQuadraticRoots provides more reliable and accurate results, particularly in edge cases 
// where precision is crucial.
bool solveQuadraticRoots(float a, float b, float c, out vec2 t) {
  float discriminant = (b * b) - ((a * c) * 4.0);
  if(discriminant < 0.0){
    t = vec2(0.0);
    return false;
  }else{
    if (discriminant == 0.0) {
      t = vec2(((-0.5 * b) / a));
    } else {
      float q = (b + (sqrt(discriminant) * (b > 0.0 ? 1.0 : -1.0))) * (-0.5);
      t = vec2(q / a, c / q);
      if(t.x > t.y){
        t = t.yx;
      }
    }  
    return true;
  }
}
#else
bool solveQuadraticRoots(float a, float b, float c, out vec2 t) {
  float discriminant = (b * b) - ((a * c) * 4.0);
  if(discriminant < 0.0){
    t = vec2(0.0);
    return false;
  }else{
    float a2 = a * 2.0;
    if(abs(a2) < 1e-7){
      t = vec2(0.0);
      return false;
    }else{
      float inverseDenominator = 1.0 / a2;
      if(abs(discriminant) < 1e-7){
        t = vec2((-b) * inverseDenominator);
      }else{
        t = fma(vec2(sqrt(discriminant)), vec2(-1.0, 1.0), vec2(-b)) * inverseDenominator;
        if(t.x > t.y){
          t = t.yx;
        }
      }
      return true;
    }
  }
}
#endif

bool intersectRaySphere(vec4 sphere, vec3 rayOrigin, vec3 rayDirection, out float time) {
  vec3 sphereCenterToRayOrigin = rayOrigin - sphere.xyz;
  vec2 t;
  bool result = solveQuadraticRoots(dot(rayDirection, rayDirection), 
                                    dot(rayDirection, sphereCenterToRayOrigin) * 2.0, 
                                    dot(sphereCenterToRayOrigin, sphereCenterToRayOrigin) - (sphere.w * sphere.w), 
                                    t);
  if(result){
    if(t.x > t.y){
      t = t.yx;
    }
    if(t.x < 0.0){
      t.x = t.y;
      if(t.x < 0.0){
        result = false;
      }
    }    
    time = t.x;
  }
  return result;
}

float getWaves(vec2 position, int iterations) {
  vec2 frequencyTimeMultiplier = vec2(1.0, 2.0);
  float weight = 1.0;
  vec2 result = vec2(0.0);
  float time = pushConstants.time; 
  float r = 0.0; 
  for(int i = 0; i < iterations; i++) {
    vec2 p = sin(vec2(r) + vec2(0.0, 1.5707963267948966));
    vec2 sinCosX = sin((vec2(dot(p, position) * frequencyTimeMultiplier.x) + (time * frequencyTimeMultiplier.y)) + vec2(0.0, 1.5707963267948966));
    vec2 res = exp(sinCosX.x - 1.0) * vec2(1.0, -sinCosX.y);
    position += p * res.y * weight * 0.38;
    result += vec2(res.x, 1.0) * weight;
    weight = mix(weight, 0.0, 0.2);
    frequencyTimeMultiplier *= vec2(1.18, 1.07);
    r += 1232.399963;
  }
  return result.x / result.y;
}

float mapEx(vec3 p, int i){
  vec3 n = normalize(planetCenter - p);
  vec3 nc = mix(n, vec3(1e-6), lessThan(abs(n), vec3(1e-6)));
  vec2 uv = vec2((atan(abs(nc.x / nc.z)) / 6.283185307179586476925286766559) + 0.5, acos(n.y) / 3.1415926535897932384626433832795); 
  float w = getWaves(uv * 128.0, i) * 0.01; 
  float h = 0.75 + w;//textureBicubicOctahedralMap(uImageHeightMap, n).x + w;
  float r = length(planetCenter - p) - mix(planetBottomRadius, planetTopRadius, h);
  return r;
}

float map(vec3 p){
  return mapEx(p, 12);
}

vec3 mapNormal(vec3 p) {
  vec2 e = vec2(1e-2, 0.0); // 0.01 meters for now for the epsilon for the normal calculation
  const int i = 37;
  return normalize(
    vec3(
      mapEx(p + e.xyy, i) - mapEx(p - e.xyy, i),
      mapEx(p + e.yxy, i) - mapEx(p - e.yxy, i),
      mapEx(p + e.yyx, i) - mapEx(p - e.yyx, i)
    )  
  );
}

const int MAX_MARCHING_STEPS = 256;

const float PRECISION = 1e-2;

float INFINITY = uintBitsToFloat(0x7f800000u); 

int countSteps = 0;

// Accelerated ray marching based on https://www.researchgate.net/publication/329152815_Accelerating_Sphere_Tracing
bool acceleratedRayMarching(vec3 rayOrigin, vec3 rayDirection, float startTime, float maxTime, float w, float q, out float hitTime){
  float previousR = 0.0; 
  float currentR = 0.0;
  float nextR = INFINITY;
  float stepDistance = 0.0;
  float time = startTime;
#if 0
  vec2 closest = vec2(INFINITY, 0.0);    
#endif
  float raySign = (map(rayOrigin) < 0.0) ? -1.0 : 1.0;  
  for(int i = 0; (i < MAX_MARCHING_STEPS) && (nextR >= PRECISION) && (time < maxTime); i++){
    float currentSignedDistance = map(fma(rayDirection, vec3(time + stepDistance), rayOrigin)) * raySign;
#if 0
    if(closest.x > abs(currentSignedDistance)){
      closest = vec2(abs(currentSignedDistance), time);
    }
#endif
    nextR = currentSignedDistance;
    if(stepDistance > (currentR + nextR)){
      stepDistance = currentR;
      currentSignedDistance = map(fma(rayDirection, vec3(time + stepDistance), rayOrigin)) * raySign;
      nextR = currentSignedDistance;
    }
    time += stepDistance;
    previousR = currentR;
    currentR = nextR * q;
    stepDistance = currentR + ((w * currentR) * (((stepDistance - previousR) + currentR) / ((stepDistance + previousR) - currentR)));
    countSteps++;
  }    
  bool hit = false;
  if((time <= maxTime) && (nextR < PRECISION)){
    hit = true;
    hitTime = min(time, maxTime);    
#if 0
  }else if((closest.x < 1e-2) && (closest.y <= maxTime)){
    hit = true;
    hitTime = closest.y;
#endif
  }
  return hit;
}

bool standardRayMarching(vec3 rayOrigin, vec3 rayDirection, float startTime, float maxTime, out float hitTime){

  bool hit = false;
  
  float t = startTime;

  float timeStep = max(1.0, maxTime) / (float(MAX_MARCHING_STEPS) * 0.125); 
  
  float closest = INFINITY;
  float closestT = 0.0;

  float secondClosest = INFINITY;
  float secondClosestT = 0.0;
  float previousDT = 0.0;

  float raySign = (map(rayOrigin) < 0.0) ? -1.0 : 1.0;  

  for(int i = 0; (i < MAX_MARCHING_STEPS) && (t < maxTime); i++){

    vec3 rayPosition = fma(rayDirection, vec3(t), rayOrigin);

    float dt = map(rayPosition) * raySign;
    if(dt < closest){
      closest = dt;
      closestT = t;
    }
    
    if((secondClosest > dt) && (previousDT < dt)){
        secondClosest = dt;
        secondClosestT = t;
    }      

    if(dt < PRECISION){
      hit = true;
      hitTime = t;
      break;
    }    

    t += (clamp(abs(dt) * 0.9, 1e-6, timeStep) * ((dt < 0.0) ? -1.0 : 1.0));
    
    previousDT = dt;

    countSteps++;
    
  }       
  
#if 0
  if((!hit) && (closest < 1e-2)){
    hit = true;
    hitTime = closestT;
  }
#endif

  return hit;

} 

#endif // PLANET_WATER_GLSL