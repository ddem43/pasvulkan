(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.FrameGraph,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.SkyBox;

type { TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass }
     TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass=class(TpvFrameGraph.TCustomPass)
      private
       fInstance:TpvScene3DRendererInstance;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass }

constructor TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='GlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass';
end;

destructor TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.AcquirePersistentResources;
begin
 inherited AcquirePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.ReleasePersistentResources;
begin
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.AcquireVolatileResources;
begin
 inherited AcquireVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.ReleaseVolatileResources;
begin
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingVisualClearCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   length(BufferMemoryBarriers),@BufferMemoryBarriers[0],
                                   0,nil);

{Size:=((fInstance.Renderer.GlobalIlluminationVoxelGridSize*
         fInstance.Renderer.GlobalIlluminationVoxelGridSize*
         fInstance.Renderer.GlobalIlluminationVoxelGridSize)*
        fInstance.Renderer.GlobalIlluminationVoxelCountClipMaps)*SizeOf(TVkUInt32);//}

 aCommandBuffer.CmdFillBuffer(fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,0,VK_WHOLE_SIZE,0);

 aCommandBuffer.CmdFillBuffer(fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,0,VK_WHOLE_SIZE,0);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                   0,
                                   0,nil,
                                   length(BufferMemoryBarriers),@BufferMemoryBarriers[0],
                                   0,nil);

end;

end.